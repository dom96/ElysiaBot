{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, OverloadedStrings #-}
module Plugins where
import System.IO
import System.IO.Error (try, catch, isEOFError)
import System.Process
import System.FilePath
import System.Directory

import Control.Concurrent
import Control.Concurrent.MVar (MVar)
import Control.Monad
import Control.Applicative
import Control.Exception (IOException)

import Data.List (isPrefixOf)
import Data.Maybe

import Network.SimpleIRC

import Types

import Text.JSON
import Text.JSON.String
import Text.JSON.Generic

import qualified Data.ByteString.Char8 as B

-- JSON
data PluginCommand = PluginCommand
  | PCMessage IrcMessage MIrc
  | PCCmdMsg  IrcMessage MIrc String String -- IrcMessage, Server, prefix, (msg without prefix)
  | PCQuit

-- Reading JSON
-- Messages that can be received from the plugins
data Message = 
   MsgSend
    { servAddr :: String
    , rawMsg   :: String
    }
  | MsgCmdAdd
    { command  :: String }
  | MsgPid
    { pid      :: Int }
  deriving Show

instance JSON Message where
  readJSON (JSObject jsonObjects) = 
    case theType of 
      (Ok "send") -> readSend objects
      (Ok "cmdadd") -> readCmdadd objects
      (Ok "pid") -> readPid objects
      (Error err) -> Error $ err
      otherwise -> Error $ "Invalid type."
    where objects = fromJSObject jsonObjects
          theType = readType $ objects !! 0
          
  showJSON = undefined

readType ("type", (JSString theType)) = Ok $ fromJSString theType
readType (name, _) = Error $ "Couldn't find 'type' or type is not a string, got " ++ name

readSend objects =
  if fst fstObj == "server" && fst sndObj == "msg" && isJust serv && isJust msg 
    then Ok $ MsgSend (fromJust serv) (fromJust msg)
    else Error "Invalid objects"
  where fstObj = objects !! 1
        sndObj = objects !! 2
        serv   = takeS $ snd fstObj 
        msg    = takeS $ snd sndObj

readCmdadd objects =
  if fst fstObj == "cmd" && isJust cmd
    then Ok $ MsgCmdAdd (fromJust cmd)
    else Error "Invalid objects"
  where fstObj = objects !! 1
        cmd    = takeS $ snd fstObj 

readPid objects =
  if fst fstObj == "pid" && isJust pid
    then Ok $ MsgPid (read $ fromJust pid)
    else Error "Invalid objects"
  where fstObj = objects !! 1
        pid    = takeS $ snd fstObj 

takeS (JSString s) = Just $ fromJSString s
takeS _            = Nothing

-- Commands -> JSON String
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

showJSONCommand (PCCmdMsg msg serv prefix cmd) = do
  servJSON <- showJSONMIrc serv
  return $ JSObject $ toJSObject $
    [("type", showJSON ("cmd" :: String))
    ,("IrcMessage", toJSON msg)
    ,("IrcServer", servJSON)
    ,("prefix", showJSON prefix)
    ,("cmd", showJSON prefix)
    ]

showJSONCommand (PCQuit) = do
  return $ JSObject $ toJSObject $
    [("type", showJSON ("quit" :: String))]

-- END OF JSON -------------------------------------------

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
  hSetBuffering outH LineBuffering
  hSetBuffering errH LineBuffering
  hSetBuffering inpH LineBuffering

  -- TODO: read the plugin.ini file.
  newMVar $ 
    Plugin plDir "" [] outH errH inpH pid Nothing [] []

getAllLines :: Handle -> IO [String]
getAllLines h = liftA2 (:) first rest `catch` (\_ -> return []) 
  where first = hGetLine h
        rest = getAllLines h

getErrs :: Plugin -> IO String
getErrs plugin = do
  -- hGetContents is lazy, getAllLines is a non-lazy hGetContents :D
  contents <- getAllLines (pStderr plugin)
  return $ unlines contents

pluginLoop :: MVar MessageArgs -> MVar Plugin -> IO ()
pluginLoop mArgs mPlugin = do
  plugin <- readMVar mPlugin
  
  -- This will wait until some output appears, and let us know when
  -- stdout is EOF
  outEof <- hIsEOF (pStdout plugin)

  if not outEof
    then do
      line <- hGetLine (pStdout plugin)
      putStrLn $ "Got line from plugin(" ++ pName plugin ++ "): " ++ line
      
      when ("{" `isPrefixOf` line) $ do 
        let decoded = decode line :: Result Message

        case decoded of
          Ok (MsgSend addr msg) -> sendRawToServer mArgs addr msg
          Ok (MsgPid pid)    -> do _ <- swapMVar mPlugin (plugin {pPid = Just pid}) 
                                   return ()
          Ok (MsgCmdAdd cmd) -> do _ <- swapMVar mPlugin (plugin {pCmds = cmd:pCmds plugin}) 
                                   return ()
          Error err       -> putStrLn $ pName plugin ++ ": JSON Error: " ++ err
      
      pluginLoop mArgs mPlugin
    else do
      -- Get the error message
      errs <- getErrs plugin

      -- Plugin crashed
      putStrLn $ "WARNING: Plugin(" ++ pName plugin ++ ") crashed, " ++ 
                 errs
      args <- takeMVar mArgs
      let filt = filter (mPlugin /=) (plugins args)
      putMVar mArgs (args {plugins = filt})

sendRawToServer :: MVar MessageArgs -> String -> String -> IO ()
sendRawToServer mArgs server msg = do
  args <- readMVar mArgs 
  servers <- readMVar $ argServers args
  filtered <- filterM (\srv -> do addr <- getAddress srv
                                  return $ addr == (B.pack server)) 
                         servers
  if not $ null filtered
    then sendRaw (filtered !! 0) (B.pack msg)
    else -- TODO: Make it report the error to the plugin
         return ()

writeCommand :: PluginCommand -> MVar Plugin -> IO ()
writeCommand cmd mPlugin = do
  plugin <- readMVar mPlugin
  (JSObject json) <- showJSONCommand cmd
  --putStrLn $ "Sending to plugin: " ++ (showJSObject json) ""
  hPutStrLn (pStdin plugin) ((showJSObject json) "")
  
messagePlugin :: MVar MessageArgs -> EventFunc
messagePlugin mArgs s m = do
  args <- readMVar mArgs
  mapM_ (writeCommand (PCMessage m s)) (plugins args)

