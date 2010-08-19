{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar

import Modules

prefix = "|"


isCmd m cmd = (prefix `B.append` cmd) `B.isPrefixOf` m 

onMessage :: MVar [IrcModule] -> EventFunc
onMessage plsMVar s m
  | msg `isCmd` "hai" = do
    sendMsg s chan "hai thar!"
  | msg `isCmd` "say" = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | msg `isCmd` "clear" = do
    _ <- swapMVar plsMVar []
    sendMsg s chan "Modules cleared"
  | msg `isCmd` "modules" = do
    mods <- peekMVar plsMVar
    sendMsg s chan ("Loaded modules: " `B.append` (B.pack $ toString mods))
  
  | B.isPrefixOf prefix msg = do
    -- If no commands are defined for this command
    -- check if they are defined in the modules
    mods <- peekMVar plsMVar
    ret <- callCmds (Just prefix) m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
  | otherwise = do
    mods <- peekMVar plsMVar
    ret <- callCmds Nothing m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
  
    return ()
  where chan = fromJust $ mChan m
        msg = mMsg m

peekMVar :: MVar a -> IO a
peekMVar m = do
  pls <- takeMVar m
  putMVar m pls
  return pls

freenode = IrcConfig 
  "irc.freenode.net" -- Address
  6667 -- Port
  "ElysiaBot" -- Nickname
  "elysia"  -- Username
  "elysia" -- Realname
  ["#()", "##XAMPP"] -- Channels to join on connect

main = do
  (modErrs, mods) <- loadMods "modules"
  putStrLn $ show $ length mods
  plsMVar <- newEmptyMVar
  putMVar plsMVar mods
  
  -- print any module loading errors.
  mapM (putStrLn . prettyError) modErrs
  
  let events = [(Privmsg (onMessage plsMVar))]
  connect (freenode events) False
