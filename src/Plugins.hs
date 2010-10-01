{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable #-}
module Plugins where
import System.IO
import System.Process
import System.FilePath
import System.Directory

import Control.Concurrent
import Control.Concurrent.MVar (MVar)
import Control.Monad

import Data.List (isPrefixOf)

import Network.SimpleIRC

import Types

import Text.JSON
import Text.JSON.String
import Text.JSON.Generic

data PluginCommand = PluginCommand
  | PCMessage IrcMessage MIrc
  | PCQuit
  
{- 
showJSONMaybe (Just s) = showJSON s
showJSONMaybe Nothing  = JSNull


instance JSON IrcMessage where
  showJSON m = JSObject $ toJSObject $ 
    [("nick", showJSONMaybe (mNick m))
    ,("user", showJSONMaybe (mUser m))
    ,("host", showJSONMaybe (mHost m))
    ,("server", showJSONMaybe (mServer m))
    ,("code", showJSON (mCode m))
    ,("msg", showJSON (mMsg m))
    ,("chan", showJSONMaybe (mChan m))
    ,("other", showJSONMaybe (mOther m))
    ,("raw", showJSON (mRaw m))
    ]

  readJSON = undefined -- TODO: ?

-}

deriving instance Data IrcMessage

showJSONMIrc s = do
  addr <- getAddress s
  nick <- getNickname s
  user <- getUsername s
  chans <- getChannels s
  
  return $ JSObject $ toJSObject $
    [("address", showJSON $ addr)
    ,("nickname", showJSON $ nick)
    ,("username", showJSON $ user)
    ,("chans", showJSON $ chans)
    ]

showJSONCommand (PCMessage msg serv) = do
  servJSON <- showJSONMIrc serv
  return $ JSObject $ toJSObject $
    [("type", showJSON ("recv" :: String))
    ,("IrcMessage", toJSON msg)
    ,("IrcServer", servJSON)
    ]

showJSONCommand (PCQuit) = do
  return $ JSObject $ toJSObject $
    [("type", showJSON ("quit" :: String))]

isCorrectDir dir f = do
  r <- doesDirectoryExist (dir </> f)
  return $ r && f /= "." && f /= ".." && not ("." `isPrefixOf` f)

runPlugins :: IO [MVar Plugin]
runPlugins = do
  contents  <- getDirectoryContents "Plugins/"
  fContents <- filterM (isCorrectDir "Plugins/") contents
  
  mapM (runPlugin) fContents
  
runPlugin :: String -> IO (MVar Plugin)
runPlugin plDir = do
  currWorkDir <- getCurrentDirectory
  let plWorkDir = currWorkDir </> "Plugins/" </> plDir
      shFile    = plWorkDir </> "run.sh"
  putStrLn $ "-- " ++ plWorkDir
  (inpH, outH, errH, pid) <- runInteractiveProcess ("./run.sh") [] (Just plWorkDir) Nothing

  -- TODO: read the plugin.ini file.
  newMVar $ 
    Plugin plDir "" [] outH errH inpH pid []
    
pluginLoop :: MVar MessageArgs -> MVar Plugin -> IO ()
pluginLoop args mPlugin = do
  plugin <- readMVar mPlugin
  line <- hGetLine (pStdout plugin)
  
  putStrLn $ "Got line from plugin(" ++ pName plugin ++ "): " ++ line
  -- TODO: Check what message has been received.

  pluginLoop args mPlugin

writeCommand :: PluginCommand -> MVar Plugin -> IO ()
writeCommand cmd mPlugin = do
  plugin <- readMVar mPlugin
  (JSObject json) <- showJSONCommand cmd
  hPutStrLn (pStdin plugin) ((showJSObject json) "")
  
messagePlugin :: MVar MessageArgs -> EventFunc
messagePlugin mArgs s m = do
  args <- readMVar mArgs
  mapM_ (writeCommand (PCMessage m s)) (plugins args)

