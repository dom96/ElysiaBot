{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar
import Language.Haskell.Interpreter (InterpreterError)
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
  | msg `isCmd` "reload"  = do
    (modErrs, mods) <- loadMods "modules"
    if null modErrs
      then do plsMVar <- newMVar mods
              sendMsg s chan "Modules reloaded succesfully."
      else do sendMsg s chan "Errors occured while reloading modules. Aborting."
              mapM (putStrLn . prettyError) modErrs
              return ()
              
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

loadModsMVar :: String -> IO ([InterpreterError], MVar [IrcModule])
loadModsMVar modDir = do
  (modErrs, mods) <- loadMods modDir
  plsMVar <- newEmptyMVar
  putMVar plsMVar mods
  return (modErrs, plsMVar)

freenode = IrcConfig 
  "irc.freenode.net" -- Address
  6667 -- Port
  "ElysiaBot" -- Nickname
  "elysia"  -- Username
  "elysia" -- Realname
  ["#()", "#HSBotTest"] -- Channels to join on connect

ninthbit = IrcConfig 
  "irc.ninthbit.net" -- Address
  6667 -- Port
  "ElysiaBot" -- Nickname
  "elysia"  -- Username
  "elysia" -- Realname
  ["#programming", "#bots"] -- Channels to join on connect

main = do
  (modErrs, plsMVar) <- loadModsMVar "modules"
  
  -- print any module loading errors.
  mapM (putStrLn . prettyError) modErrs
  
  let events = [(Privmsg (onMessage plsMVar))]
  --connect (ninthbit events) True False
  connect (freenode events) False True
