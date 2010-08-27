{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.Console.GetOpt
import System.Environment
import System.Posix.Daemonize
import System.Exit

import Modules
import Config
import Users
import Commands

data Options = Options
 {  
   optDaemon      :: Bool
 , optShowVersion :: Bool
 } deriving Show
 
defaultOptions    = Options
 { optDaemon      = False
 , optShowVersion = False
 }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['v'] ["version"] 
        (NoArg (\opts -> opts { optShowVersion = True })) "show version number"
    , Option ['d'] ["daemon"] 
        (NoArg (\opts -> opts { optDaemon = True })) "Run as Daemon"
    ]

parseOpts :: Options -> IO ()
parseOpts xs
    | optShowVersion xs = do putStrLn "ElysiaBot 0.1.0"
                             exitSuccess
    | otherwise         = return ()

header = "Usage: elysia [options]"

elOpts :: [String] -> IO (Options, [String])
elOpts args =
    case getOpt RequireOrder options args of 
        (xs, n, []) -> return (foldl (flip id) defaultOptions xs, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

confToIRCConf :: String -> String -> String -> ConfigServer -> IrcConfig
confToIRCConf nick usr real confServ =
  defaultConfig
    { cAddr = (cnfAddr confServ)
    , cPort = (cnfPort confServ)
    , cNick = nick
    , cUsername = usr
    , cRealname = real
    , cChannels = (cnfChans confServ)
    }

connectServers events = do
  conf <- readConfig "elysia.ini"
  let (nick, usr, real) = ((cnfNick conf), (cnfUser conf), (cnfReal conf)) 
  let lConn t serv = connect ((confToIRCConf nick usr real serv) {cEvents = events}) t True
  mapM (lConn True) (drop 1 $ cnfServers conf)
  
  lConn False (cnfServers conf !! 0)

main2 = do
  let mods = loadMods "modules"
  
  -- Load the users
  putStrLn "Loading users"
  users <- readUsers "users.ini"
  
  -- Create the MessageArgs MVar
  argsMVar <- newMVar (MessageArgs mods users [])
  
  let events = [(Privmsg (onMessage argsMVar)), (Privmsg (onPrivateMessage argsMVar)), (Numeric (collectServers argsMVar))]
  _ <- connectServers events
  return ()

main = do
  cmdArgs <- getArgs
  (opts, _) <- elOpts cmdArgs
  parseOpts opts
  if optDaemon opts
    then daemonize main2
    else main2
  

