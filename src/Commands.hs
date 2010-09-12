{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage, onPrivateMessage, safeCheckArg, collectServers, MessageArgs(..)) where
import Network.SimpleIRC
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar
import Control.Concurrent
import System.Exit
import System.Process
import System.FilePath

import Modules
import Users

prefix = "|" -- Move this to the configuration file/Types.hs

data MessageArgs = MessageArgs
  { modules :: [IrcModule]
  , users   :: Users
  , argServers :: [MIrc]
  }
  
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

cmdHandler :: MVar MessageArgs -> MIrc -> IrcMessage -> B.ByteString -> IO ()
cmdHandler argsMVar mIrc m dest
  | msg `isCmd` "hai" = do
    sendMsg mIrc dest "hai thar!"
  | msg `isCmd` "say" = do
    sendMsg mIrc dest (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | msg `isCmd` "clear" = do
    modifyMVar_ argsMVar (\a -> return $ a {modules = []})
    sendMsg mIrc dest "Modules cleared"
  
  | msg `isCmd` "modules" = do
    args <- readMVar argsMVar
    let mods = modules args
    sendMsg mIrc dest ("Available modules: " `B.append` (B.pack $ toString mods))
  
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
  
  | (msg `isCmd` "mute" || msg `isCmd` "unmute") && safeCheckArg msg 1  = do
    args <- readMVar argsMVar
    let mod    = fromJust $ safeGetArg msg 1
    let func   = (msg `isCmd` "mute") ? (muteModule, unmuteModule)
    let result = func (modules args) mod dest
    
    ifAdmin (users args) (B.unpack $ fromJust $ mNick m)
      (do if isJust result
            then do _ <- swapMVar argsMVar (args {modules = fromJust $ result})
                    sendMsg mIrc dest 
                      ("Module successfully " `B.append`
                      ((msg `isCmd` "mute") ? ("", "un")) `B.append` "muted.")
            else sendMsg mIrc dest "Module not found.")
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
  
  | B.isPrefixOf prefix msg = do
    -- If no commands are defined for this command
    -- check if they are defined in the modules
    args <- readMVar argsMVar
    let mods = modules args
    ret <- callCmds (Just prefix) m mods mIrc
    mapM (\plM -> sendMsg mIrc dest (plM)) (concat ret)
    
    putStrLn $ (show $ length $ concat ret) ++ " prefixed commands replied to."
    
  | otherwise = do
    args <- readMVar argsMVar
    let mods = modules args
    ret <- callCmds Nothing m mods mIrc
    mapM (\plM -> sendMsg mIrc dest (plM)) (concat ret)
    
    putStrLn $ (show $ length $ concat ret) ++ " raw commands replied to."
  
    return ()
  where msg  = mMsg m

onMessage :: MVar MessageArgs -> EventFunc
onMessage argsMVar s m = do
  dest <- getDest s m
  
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
    modifyMVar_ argsMVar (\a -> return a {argServers = s:(argServers a)})
  | otherwise = return ()
  
-- Helpers
(?) :: Bool -> (c, c) -> c
(?) b (t, e) = if b then t else e

sendAll :: [MIrc] -> Command -> IO ()
sendAll servers cmd = do
  mapM (flip sendCmd cmd) servers
  return ()

