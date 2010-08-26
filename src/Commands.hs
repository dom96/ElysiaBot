{-# LANGUAGE OverloadedStrings #-}
module Commands (onMessage, onPrivateMessage, safeCheckArg, MessageArgs(..)) where
import Network.SimpleIRC
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar
import System.Exit

import Modules
import Users

prefix = "|" -- Move this to the configuration file/Types.hs

data MessageArgs = MessageArgs
  { modules :: [IrcModule]
  , users   :: Users
  }
  
isCmd m cmd = (prefix `B.append` cmd) `B.isPrefixOf` m 

safeGetArg :: B.ByteString -> Int -> Maybe B.ByteString
safeGetArg str index
  | (length word - 1) >= index =
    Just (word !! index)
  | otherwise = Nothing
  where word = B.words str
  
safeCheckArg :: B.ByteString -> Int -> Bool
safeCheckArg str index = (length word - 1) >= index
  where word = B.words str
  
  
onMessage :: MVar MessageArgs -> EventFunc
onMessage argsMVar s m
  | msg `isCmd` "hai" = do
    sendMsg s chan "hai thar!"
  | msg `isCmd` "say" = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | msg `isCmd` "clear" = do
    modifyMVar_ argsMVar (\a -> return $ a {modules = []})
    sendMsg s chan "Modules cleared"
  
  | msg `isCmd` "modules" = do
    args <- readMVar argsMVar
    let mods = modules args
    sendMsg s chan ("Available modules: " `B.append` (B.pack $ toString mods))
  
  | msg `isCmd` "quit"  = do
    args <- readMVar argsMVar
    ifAdmin (users args) (B.unpack $ fromJust $ mNick m) exitSuccess
            (sendMsg s chan "You need to be an admin to execute this command.")
  
  | msg `isCmd` "users" = do
    args <- readMVar argsMVar
    let len    = length $ M.toList $ users args
    let admins = M.fold (\a b -> if uAdmin a then b + 1 else b) 0 (users args)
    sendMsg s chan ("I have " `B.append` (B.pack $ show len) `B.append` " users, "
                    `B.append` (B.pack $ show admins) `B.append` " of which are Admins.")
  
  | msg `isCmd` "online" && safeCheckArg msg 1  = do
    args <- readMVar argsMVar
    if checkOnline (users args) (B.unpack $ fromJust $ safeGetArg msg 1)
      then sendMsg s chan "User is online"
      else sendMsg s chan "User is offline"  
  
  | msg `isCmd` "online" = do
    args <- readMVar argsMVar
    let onUsers = getLoggedin $ users args
    if not $ null onUsers
      then sendMsg s chan 
           ("Users online: " `B.append` (B.pack $ onUsers))
      else sendMsg s chan "No users online"
  
  | B.isPrefixOf prefix msg = do
    -- If no commands are defined for this command
    -- check if they are defined in the modules
    args <- readMVar argsMVar
    let mods = modules args
    ret <- callCmds (Just prefix) m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
    
  | otherwise = do
    args <- readMVar argsMVar
    let mods = modules args
    ret <- callCmds Nothing m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
  
    return ()
  where chan = getChan s m
        msg  = mMsg m

onPrivateMessage :: MVar MessageArgs -> EventFunc
onPrivateMessage argsMVar s m
  | sNickname s == (fromJust $ mChan m) = do
    case (lineM !! 0) of
      "login"   -> changeStatus argsMVar True s m 
      "logout"  -> changeStatus argsMVar False s m
      otherwise -> return ()
    
  | otherwise = return ()
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
        

(?) :: Bool -> (c, c) -> c
(?) b (t, e) = if b then t else e

-- If a private message(/msg) is received then returns the nick, otherwise the chan
getChan :: IrcServer -> IrcMessage -> B.ByteString
getChan s m =
  if sNickname s == chan
    then (fromJust $ mNick m)
    else chan
  
  where chan = fromJust $ mChan m

