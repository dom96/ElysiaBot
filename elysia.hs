{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar
import Language.Haskell.Interpreter (InterpreterError)
import Modules
import Config

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
    mods <- readMVar plsMVar
    sendMsg s chan ("Loaded modules: " `B.append` (B.pack $ toString mods))
  | msg `isCmd` "reload"  = do
    (modErrs, mods) <- loadMods "modules"
    if null modErrs
      then do _ <- swapMVar plsMVar mods
              sendMsg s chan "Modules reloaded succesfully."
      else do sendMsg s chan "Errors occured while reloading modules. Aborting."
              mapM (putStrLn . prettyError) modErrs
              return ()
              
  | B.isPrefixOf prefix msg = do
    -- If no commands are defined for this command
    -- check if they are defined in the modules
    mods <- readMVar plsMVar
    ret <- callCmds (Just prefix) m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
    
  | otherwise = do
    mods <- readMVar plsMVar
    ret <- callCmds Nothing m mods
    mapM (\plM -> sendMsg s chan (plM)) (concat ret)
    
    putStrLn $ show $ length $ concat ret
  
    return ()
  where chan = fromJust $ mChan m
        msg = mMsg m

loadModsMVar :: String -> IO ([InterpreterError], MVar [IrcModule])
loadModsMVar modDir = do
  (modErrs, mods) <- loadMods modDir
  plsMVar <- newMVar mods
  return (modErrs, plsMVar)

confToIRCConf :: String -> String -> String -> ConfigServer -> ([IrcEvent] -> IrcConfig)
confToIRCConf nick usr real confServ =
  IrcConfig (cnfAddr confServ) (cnfPort confServ) nick usr real (cnfChans confServ)

connectServers events = do
  conf <- readConfig "elysia.ini"
  let (nick, usr, real) = ((cnfNick conf), (cnfUser conf), (cnfReal conf)) 
  let lConn t serv = connect (confToIRCConf nick usr real serv events) t True
  mapM (lConn True) (drop 1 $ cnfServers conf)
  
  lConn False (cnfServers conf !! 0)

main = do
  (modErrs, plsMVar) <- loadModsMVar "modules"
  
  -- print any module loading errors.
  mapM (putStrLn . prettyError) modErrs
  
  let events = [(Privmsg (onMessage plsMVar))]
  connectServers events
