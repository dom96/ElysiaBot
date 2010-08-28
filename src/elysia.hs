{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Posix.Daemonize

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

connectServers events conf = do
  let (nick, usr, real) = ((cnfNick conf), (cnfUser conf), (cnfReal conf)) 
  let lConn t serv = connect ((confToIRCConf nick usr real serv) {cEvents = events}) t True
  mapM (lConn True) (drop 1 $ cnfServers conf)
  
  lConn False (cnfServers conf !! 0)

main = do
  cmdArgs <- getArgs
  (opts, _) <- elOpts cmdArgs
  parseOpts opts
  
  let mods = loadMods "modules"
  
  -- Load the users
  putStrLn "Loading users"
  users <- readUsers "users.ini"
  
  -- Load the configuration
  putStrLn "Loading configuration"
  conf <- readConfig "elysia.ini"
  
  -- Create the MessageArgs MVar
  argsMVar <- newMVar (MessageArgs mods users [])
  
  let events = [(Privmsg (onMessage argsMVar)), (Privmsg (onPrivateMessage argsMVar)), (Numeric (collectServers argsMVar))]
  let connect' = do _ <- connectServers events conf
                    return ()
  
  -- All file loading(that is in elysia's directory) has to be done before calling
  -- daemonize, because it changes the current working dir.
  -- TODO:(Maybe) I suppose i could just get the current working directory
  -- before calling daemonize, that's messy though. This way is cleaner.
  -- Just remember the rule, all files must be loaded before here.
  
  if optDaemon opts
    then daemonize connect'
    else connect'
  

