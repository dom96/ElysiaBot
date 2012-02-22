{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import System.IO (writeFile, readFile)
import System.Exit
import System.FilePath ((</>))
import System.Directory
import System.Environment
import System.Posix.Signals
import System.Posix.Process (getProcessID)
import System.Console.GetOpt
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Config
import Users
import Commands
import Plugins

data Options = Options
 {  
   optShowVersion :: Bool
 , optStop        :: Bool
 } deriving Show
 
defaultOptions    = Options
 { optShowVersion = False
 , optStop        = False
 }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option ['v'] ["version"] 
        (NoArg (\opts -> opts { optShowVersion = True })) "Show version number"
    , Option ['S'] ["stop"]
        (NoArg (\opts -> opts { optStop = True })) "Stop"
    ]

parseOpts :: Options -> IO ()
parseOpts xs
    | optShowVersion xs = do putStrLn "ElysiaBot 0.1.0"
                             exitSuccess
    | optStop xs        = do stopElysia
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

onDisconnect mArgs mIrc = do
  m <- reconnect mIrc
  either (\err -> putStrLn $ "Unable to reconnect: " ++ show err)
         (\_   -> putStrLn "Successfully reconnected.")
         m

writePID conf = do
  pid <- getProcessID
  writeFile (cnfPidFile conf) (show pid)

isRunning conf = do
  exists <- doesFileExist (cnfPidFile conf)
  if exists
    then do putStrLn "Elysia is already running.\nUse ./Elysia --stop to stop."
            exitSuccess
    else return ()

stopElysia = do
  appDatDir <- getAppUserDataDirectory "ElysiaBot"

  conf <- readConfig $ appDatDir </> "elysia.ini"
  pid <- readFile (cnfPidFile conf)
  catch (signalProcess sigTERM (read $ (lines pid) !! 0))
        (putStrLn . show)
  removeFile (cnfPidFile conf)
  exitSuccess

main = do
  cmdArgs <- getArgs
  (opts, _) <- elOpts cmdArgs
  parseOpts opts
  
  appDatDir <- getAppUserDataDirectory "ElysiaBot"
  
  -- Create a new MVar to store the servers
  serversMVar <- newMVar ([] :: [MIrc])
  
  -- Load the users
  putStrLn $ "Loading users - " ++ appDatDir </> "users.ini"
  users <- readUsers $ appDatDir </> "users.ini"
  
  -- Load the configuration
  putStrLn $ "Loading configuration - " ++ appDatDir </> "elysia.ini"
  conf <- readConfig $ appDatDir </> "elysia.ini"
  
  -- Load the plugins
  putStrLn $ "Loading plugins..."
  plugins <- runPlugins
  
  -- Create the MessageArgs MVar
  argsMVar <- newMVar (MessageArgs users serversMVar plugins)
  
  -- Run the pluginLoops. pluginLoop will block until the plugin sends the pid
  -- message, when it does it will run itself in a new thread.
  mapM_ (pluginLoop argsMVar) plugins
  
  let events = [(Privmsg (onMessage argsMVar))
               ,(Privmsg (onPrivateMessage argsMVar))
               ,(Numeric (collectServers argsMVar))
               ,(RawMsg  (messagePlugin argsMVar))
               ,(Disconnect (onDisconnect argsMVar))
               ]
  let connect' = do writePID conf
                    _ <- connectServers events conf
                    return ()
  
  -- All file loading(that is in elysia's directory) has to be done before calling
  -- daemonize, because it changes the current working dir.
  
  connect'
