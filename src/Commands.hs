{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage, onPrivateMessage, safeCheckArg, collectServers, MessageArgs(..)) where
import Network.SimpleIRC
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.List (find, isPrefixOf)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar
import Control.Concurrent
import System.Exit
import System.Process
import System.FilePath

import Users
import Types
import Plugins

prefix = "|" -- TODO: Move this to the configuration file/Types.hs
  
isCmd m cmd = (prefix `B.append` cmd) `B.isPrefixOf` m 

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

-- TODO: CLean up into smaller functions.

cmdHandler :: MVar MessageArgs -> MIrc -> IrcMessage -> B.ByteString -> IO ()
cmdHandler argsMVar mIrc m dest
  | msg `isCmd` "hai" = do
    sendMsg mIrc dest "hai thar!"
  | msg `isCmd` "say" = do
    sendMsg mIrc dest (B.drop 1 $ B.dropWhile (/= ' ') msg)
  
  | msg `isCmd` "quit"  = do
    args <- readMVar argsMVar
    ifAdmin (users args) (B.unpack $ fromJust $ mNick m) exitSuccess
            (sendMsg mIrc dest "You need to be an admin to execute this command.")
  
  | msg `isCmd` "join" && safeCheckArg msg 1 = do
    args <- readMVar argsMVar
    let chanToJoin = fromJust $ safeGetArg msg 1
    ifAdmin (users args) (B.unpack $ fromJust $ mNick m)
            (sendCmd mIrc $ MJoin chanToJoin Nothing)
            (sendMsg mIrc dest "You need to be an admin to execute this command.")
  
  | msg `isCmd` "users" = do
    args <- readMVar argsMVar
    let len    = length $ M.toList $ users args
    let admins = M.fold (\a b -> if uAdmin a then b + 1 else b) 0 (users args)
    sendMsg mIrc dest ("I have " `B.append` (B.pack $ show len) `B.append` " users, "
                    `B.append` (B.pack $ show admins) `B.append` " of which are Admins.")
  
  | msg `isCmd` "online" && safeCheckArg msg 1  = do
    args <- readMVar argsMVar
    if checkOnline (users args) (B.unpack $ fromJust $ safeGetArg msg 1)
      then sendMsg mIrc dest "User is online"
      else sendMsg mIrc dest "User is offline"  
  
  | msg `isCmd` "online" = do
    args <- readMVar argsMVar
    let onUsers = getLoggedin $ users args
    if not $ null onUsers
      then sendMsg mIrc dest 
           ("Users online: " `B.append` (B.pack $ onUsers))
      else sendMsg mIrc dest "No users online"
  
  | msg `isCmd` "plugins" = do
    args <- readMVar argsMVar
    let lenPlugins = length $ plugins args
    if lenPlugins == 0
      then sendMsg mIrc dest "No plugins loaded."
      else sendMsg mIrc dest ((B.pack $ show lenPlugins) `B.append` " plugin(s) loaded.")
  
  | msg `isCmd` "desc" && safeCheckArg msg 1 = do
    args <- readMVar argsMVar
    let name   = B.unpack $ fromJust $ safeGetArg msg 1
    plugin <- findPlugin argsMVar (B.pack name)
    if isJust plugin
      then sendMsg mIrc dest (pDescription (fromJust plugin))
      else sendMsg mIrc dest "Plugin not found."
    
  | otherwise = return ()
  where msg  = mMsg m

dropPrefix :: IrcMessage -> B.ByteString
dropPrefix m = B.drop (B.length prefix) (mMsg m)

pluginHasCmd :: IrcMessage -> MVar Plugin -> IO (Either () B.ByteString)
pluginHasCmd msg mPlugin = do
  plugin <- readMVar mPlugin
  let cmd      = dropPrefix msg
      maybeCmd = find ((flip B.isPrefixOf) cmd) (pCmds plugin)
  if isJust maybeCmd
    then return $ Right $ fromJust maybeCmd
    else return $ Left ()


sendCmdPlugins :: MVar MessageArgs -> MIrc -> IrcMessage -> IO ()
sendCmdPlugins mArgs s msg = do
  -- Check if any plugin has this command binded
  args <- readMVar mArgs
  if B.isPrefixOf prefix (mMsg msg)
    then mapM_ (condWrite) (plugins args)
    else return ()

  where doWrite p cmd = do
          let rest  = B.drop (B.length prefix + B.length cmd) (mMsg msg)
              rest2 = if B.take 1 rest == " "
                        then B.drop 1 rest
                        else rest
          writeCommand (PCCmdMsg msg s prefix cmd rest2) p
        condWrite p = do
          cmd <- pluginHasCmd msg p
          either (\_ -> return ()) (doWrite p) cmd

onMessage :: MVar MessageArgs -> EventFunc
onMessage argsMVar s m = do
  dest <- getDest s m
  
  sendCmdPlugins argsMVar s m
  
  cmdHandler argsMVar s m dest


onPrivateMessage :: MVar MessageArgs -> EventFunc
onPrivateMessage argsMVar s m = do
  nick <- getNickname s
  if nick == (fromJust $ mChan m)
    then do case (lineM !! 0) of
              "login"   -> changeStatus argsMVar True s m 
              "logout"  -> changeStatus argsMVar False s m
              otherwise -> return ()
    
    else return ()
  where msg   = mMsg m
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
    
  where msg      = mMsg m
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
    modifyMVar_ (argServers args) (\a -> return $ s:a)
  | otherwise = return ()
  
-- Helpers
(?) :: Bool -> (c, c) -> c
(?) b (t, e) = if b then t else e

sendAll :: [MIrc] -> Command -> IO ()
sendAll servers cmd = do
  mapM (flip sendCmd cmd) servers
  return ()

