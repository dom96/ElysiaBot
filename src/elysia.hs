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
import Users
import Commands

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
  let mods = loadMods "modules"
  
  -- Load the users
  putStrLn "Loading users"
  users <- readUsers "users.ini"
  
  -- Create the MessageArgs MVar
  argsMVar <- newMVar (MessageArgs mods users)
  
  let events = [(Privmsg (onMessage argsMVar)), (Privmsg (onPrivateMessage argsMVar))]
  connectServers events
