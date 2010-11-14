{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage, onPrivateMessage, safeCheckArg, collectServers, MessageArgs(..)) where
import Network.SimpleIRC
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (find, isPrefixOf, intercalate)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.Exit
import System.Process
import System.FilePath

import Users
import Types
import Plugins

prefix = "|" -- TODO: Move this to the configuration file/Types.hs

startsWithPrefix :: B.ByteString -> B.ByteString -> Maybe B.ByteString
startsWithPrefix myNick m =
    if prefixedCmd
      then Just prefix
      else directCmd
  where 
    prefixedCmd = (prefix) `B.isPrefixOf` m 
    directCmd
      | (myNick `B.append` " ") `B.isPrefixOf` m  = 
          Just (myNick `B.append` " ")
      | (myNick `B.append` ": ") `B.isPrefixOf` m = 
          Just (myNick `B.append` ": ")
      | (myNick `B.append` ", ") `B.isPrefixOf` m = 
          Just (myNick `B.append` ", ")
      | (myNick `B.append` ":") `B.isPrefixOf` m  = 
          Just (myNick `B.append` ":")
      | (myNick `B.append` ",") `B.isPrefixOf` m  = 
          Just (myNick `B.append` ",")
      | otherwise = Nothing

isCmd :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
isCmd prfx m cmd = (prfx `B.append` cmd) `B.isPrefixOf` m

safeGetArg :: B.ByteString -> Int -> Maybe B.ByteString
safeGetArg str index
  | (length word - 1) >= index =
    Just (word !! index)
  | otherwise = Nothing
  where word = B.words str

-- |Checks if index exists.
safeCheckArg :: B.ByteString -> Int -> Bool
safeCheckArg str index = (length word - 1) >= index
  where word = B.words str

sendNeedAdmin :: MIrc -> B.ByteString -> IO ()
sendNeedAdmin mIrc origin = 
  sendMsg mIrc origin "You need to be an admin to execute this command."

ifAdmin_ :: MVar MessageArgs -> MIrc -> IrcMessage -> IO () -> IO ()
ifAdmin_ mArgs mIrc m f = do 
  args <- readMVar mArgs
  ifAdmin (users args) nick f (sendNeedAdmin mIrc origin)
  where nick   = B.unpack $ fromJust $ mNick m
        origin = fromJust $ mOrigin m
-- TODO: CLean up into smaller functions.

cmdHandler :: MVar MessageArgs -> B.ByteString -> MIrc -> IrcMessage -> IO ()
cmdHandler mArgs prfx mIrc m
    | msg `isCmd'` "say" = do
      sendMsg mIrc origin (B.drop (B.length prfx) $ B.dropWhile (/= ' ') msg)
    
    | msg `isCmd'` "quit"  = do
      ifAdmin_ mArgs mIrc m exitSuccess
    
    | msg `isCmd'` "join" && safeCheckArg msg 1 = do
      let chanToJoin = fromJust $ safeGetArg msg 1
      ifAdmin_ mArgs mIrc m (sendCmd mIrc $ MJoin chanToJoin Nothing)
    
    | msg `isCmd'` "part" && safeCheckArg msg 1 = do
      let chanToPart = fromJust $ safeGetArg msg 1
      ifAdmin_ mArgs mIrc m (sendCmd mIrc $ MPart chanToPart "Leaving...")

    | otherwise = return ()
  where 
    msg       = mMsg m
    origin    = fromJust $ mOrigin m
    isCmd'    = isCmd prfx
    
cmdHandlerUsers :: MVar MessageArgs -> B.ByteString -> MIrc -> IrcMessage -> IO ()
cmdHandlerUsers mArgs prfx mIrc m 
    | msg `isCmd'` "users" = do
      args <- readMVar mArgs
      let len    = length $ M.toList $ users args
      let admins = M.fold (\a b -> if uAdmin a then b + 1 else b) 0 (users args)
      sendMsg mIrc origin ("I have " `B.append` (B.pack $ show len) `B.append` " users, "
                      `B.append` (B.pack $ show admins) `B.append` " of which are Admins.")
    
    | msg `isCmd'` "online" && safeCheckArg msg 1  = do
      args <- readMVar mArgs
      if checkOnline (users args) (B.unpack $ fromJust $ safeGetArg msg 1)
        then sendMsg mIrc origin "User is online"
        else sendMsg mIrc origin "User is offline"  
    
    | msg `isCmd'` "online" = do
      args <- readMVar mArgs
      let onUsers = getLoggedin $ users args
      if not $ null onUsers
        then sendMsg mIrc origin 
             ("Users online: " `B.append` (B.pack $ onUsers))
        else sendMsg mIrc origin "No users online"
    
    | otherwise = return ()
  where 
    msg       = mMsg m
    origin    = fromJust $ mOrigin m
    isCmd'    = isCmd prfx

cmdHandlerPlugins :: MVar MessageArgs -> B.ByteString -> MIrc -> IrcMessage -> IO ()
cmdHandlerPlugins mArgs prfx mIrc m
    | msg `isCmd'` "plugins" = do
      pluginsStr <- listPlugins mArgs
      if isJust pluginsStr
        then sendMsg mIrc origin ("Plugins loaded: " `B.append` (fromJust pluginsStr))
        else sendMsg mIrc origin "No plugins loaded."
    
    | msg `isCmd'` "desc" && safeCheckArg msg 1 = do
      args <- readMVar mArgs
      let name = B.unpack $ fromJust $ safeGetArg msg 1
      desc <- getPluginProperty mArgs (B.pack name) pDescription
      if isJust desc
        then sendMsg mIrc origin (fromJust desc)
        else sendMsg mIrc origin "Plugin not found."
    
    | msg `isCmd'` "unload" && safeCheckArg msg 1 = do
      ifAdmin_ mArgs mIrc m $
        do let name = fromJust $ safeGetArg msg 1
           result <- unloadPlugin mArgs name 
           if result
             then sendMsg mIrc origin (name `B.append` " unloaded successfully.")
             else sendMsg mIrc origin "Plugin not found."
    
    | msg `isCmd'` "load" && safeCheckArg msg 1 = do
      ifAdmin_ mArgs mIrc m $
        do let name = fromJust $ safeGetArg msg 1
           result <- loadPlugin mArgs name
           if result
             then sendMsg mIrc origin (name `B.append` " loaded successfully.")
             else sendMsg mIrc origin "Plugin not found."

    | otherwise = return ()
  where 
    msg       = mMsg m
    origin    = fromJust $ mOrigin m
    isCmd'    = isCmd prfx
  
dropPrefix :: IrcMessage -> B.ByteString -> B.ByteString
dropPrefix m prfx = B.drop (B.length prfx) (mMsg m)

pluginHasCmd :: IrcMessage -> MVar Plugin -> B.ByteString -> IO (Either () B.ByteString)
pluginHasCmd msg mPlugin prfx = do
    plugin <- readMVar mPlugin
    let cmd      = dropPrefix msg prfx
        maybeCmd = find ((flip B.isPrefixOf) cmd) (pCmds plugin)
    if isJust maybeCmd
      then return $ Right $ fromJust maybeCmd
      else return $ Left ()

sendCmdPlugins :: MVar MessageArgs -> B.ByteString -> MIrc -> IrcMessage -> IO ()
sendCmdPlugins mArgs prfx s msg = do
    -- Check if any plugin has this command binded
    args <- readMVar mArgs
    mapM_ (condWrite) (plugins args)

    where doWrite p cmd = do
              let rest  = B.drop (B.length prfx + B.length cmd) (mMsg msg)
                  rest2 = if B.take 1 rest == " "
                            then B.drop 1 rest
                            else rest
              writeCommand (PCCmdMsg msg s prfx cmd rest2) p
          condWrite p = do
              cmd <- pluginHasCmd msg p prfx
              either (\_ -> return ()) (doWrite p) cmd

onMessage :: MVar MessageArgs -> EventFunc
onMessage mArgs s m = do
    myNick <- getNickname s 
    let isPrfx    = startsWithPrefix myNick (mMsg m)

    isPrivMsg <- isPM s m

    let maybePrfx = 
          if isJust isPrfx
            then isPrfx
            else if isPrivMsg then Just "" else Nothing

    when (isJust maybePrfx) $ do 
      let prfx = fromJust maybePrfx
        
      sendCmdPlugins mArgs prfx s m
      
      cmdHandler mArgs prfx s m
      cmdHandlerPlugins mArgs prfx s m
      cmdHandlerUsers mArgs prfx s m

onPrivateMessage :: MVar MessageArgs -> EventFunc
onPrivateMessage argsMVar s m = do
    isPrivMsg <- isPM s m
    when isPrivMsg $ do 
      case (lineM !! 0) of
        "login"   -> changeStatus argsMVar True s m 
        "logout"  -> changeStatus argsMVar False s m
        otherwise -> return ()
  where 
    msg   = mMsg m
    lineM = B.words msg

changeStatus :: MVar MessageArgs -> Bool -> EventFunc
changeStatus argsMVar state s m
    | length lineM >= 3 = do
      args <- readMVar argsMVar
      let usrNick = B.unpack $ lineM !! 1
      let usrPass = B.unpack $ lineM !! 2
      let newUsers = setLoggedin (users args) usrNick usrPass state
      either (\m -> sendMsg s nick (B.pack m))
             (loginSuccess args) newUsers
             
    | otherwise         = 
      sendMsg s nick ("log" `B.append` strState `B.append` " <nick> <pass>")
      
  where 
    msg      = mMsg m
    lineM    = B.words msg
    nick     = fromJust $ mNick m
    strState = (state ? ("in", "out"))
    loginSuccess args m = do 
      _ <- swapMVar argsMVar (args {users = m})
      sendMsg s nick ("You are now logged " `B.append` strState)
          
collectServers :: MVar MessageArgs -> EventFunc
collectServers argsMVar s m
    | mCode m == "001" = do
      args <- readMVar argsMVar
      servers <- readMVar (argServers args)
      -- Check whether this particular Server MVar is already in the server list.
      if not $ s `elem` servers
        then modifyMVar_ (argServers args) (\a -> return $ s:a)
        else return ()

    | otherwise = return ()

listPlugins :: MVar MessageArgs -> IO (Maybe B.ByteString)
listPlugins mArgs = do
    args <- readMVar mArgs
    pluginNames <- mapM ((flip getPluginProperty_) pName) (plugins args)
    if not $ null pluginNames
      then do str <- foldM foldFunc "" pluginNames
              return $ Just str
      else return Nothing
  where 
    foldFunc a b = do
        if not $ B.null a
            then return $ a `B.append` ", " `B.append` b
            else return $ b



-- Helpers
(?) :: Bool -> (c, c) -> c
(?) b (t, e) = if b then t else e

sendAll :: [MIrc] -> Command -> IO ()
sendAll servers cmd = do
  mapM (flip sendCmd cmd) servers
  return ()

-- |Checks if the message is a Private Message.
isPM :: MIrc -> IrcMessage -> IO Bool
isPM s m = do
  nick <- getNickname s
  return $ nick == (fromJust $ mChan m)
    
