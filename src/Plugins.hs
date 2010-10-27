{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module Plugins (PluginCommand(..), writeCommand, runPlugins, pluginLoop, messagePlugin) where
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

import Network.SimpleIRC

import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Data.Maybe
import Data.List (isPrefixOf)
import Data.Ratio (numerator)
import Data.Char (toLower)

import qualified Data.ByteString.Char8 as B

import Types

data RPC = 
    RPCRequest
      { reqMethod :: B.ByteString
      , reqParams :: JSValue
      , reqId     :: Maybe Rational
      } 
  | RPCResponse 
      { rspResult :: B.ByteString
      , rspError  :: Maybe B.ByteString
      , rspID     :: Rational
      }
  deriving (Typeable, Show)
  
data Message = 
   MsgSend
    { servAddr :: B.ByteString
    , rawMsg   :: B.ByteString
    , sId      :: Int
    }
  | MsgCmdAdd
    { command  :: B.ByteString, caId :: Int }
  | MsgPid
    { pid      :: Int }
  deriving Show
  
data PluginCommand = PluginCommand
  -- Requests
  | PCMessage IrcMessage MIrc
  | PCCmdMsg  IrcMessage MIrc String String -- IrcMessage, Server, prefix, (msg without prefix)
  | PCQuit
  -- Responses
  | PCSuccess B.ByteString Int -- result message, id
  | PCError   B.ByteString Int -- error message, id

validateFields :: JSValue -> [String] -> Bool
validateFields (JSObject obj) fields =
  let exist = map (get_field obj) fields
  in all (isJust) exist

validateArray :: [JSValue] -> Int -> Bool
validateArray arr num = length arr == num 

-- -.-
getJSString :: JSValue -> B.ByteString
getJSString (JSString (JSONString s)) = B.pack s

getJSMaybe :: JSValue -> Maybe JSValue
getJSMaybe (JSNull) = 
  Nothing
getJSMaybe jsvalue = 
  Just jsvalue

getJSRatio :: JSValue -> Rational
getJSRatio (JSRational _ r) = r
getJSRatio _ = error "Not a JSRational."

errorResult :: Result a -> a
errorResult (Ok a) = a
errorResult (Error s) = error s

-- Turns the parsed JSValue into a RPC(Either a RPCRequest or RPCResponse)
jsToRPC :: JSValue -> RPC
jsToRPC js@(JSObject obj) 
  | validateFields js ["method", "params", "id"] =
    let rID = getJSMaybe $ fromJust $ get_field obj "id" 
    in RPCRequest 
         { reqMethod = getJSString $ fromJust $ get_field obj "method"
         , reqParams = fromJust $ get_field obj "params" 
         , reqId     = if isJust $ rID 
                          then Just $ getJSRatio $ fromJust rID
                          else Nothing 
         }

  -- TODO: RPCResponse -- Currently there are no Responses from the plugin.

-- This function just checks the reqMethod of RPCRequest.
rpcToMsg :: RPC -> Either (Int, B.ByteString) Message
rpcToMsg req@(RPCRequest method _ _)
  | method == "send"    = rpcToSend   req
  | method == "cmdadd"  = rpcToCmdAdd req
  | method == "pid"     = rpcToPID    req 

-- Turns an RPC(Which must be a RPCRequest with a method of "send") into a MsgSend.
rpcToSend :: RPC -> Either (Int, B.ByteString) Message
rpcToSend (RPCRequest _ (JSArray params) (Just id))
  | validateArray params 2 = 
    let server    = getJSString $ params !! 0
        msg       = getJSString $ params !! 1
    in Right $ MsgSend server msg numId
  -- PRIVMSG, NOTICE, JOIN, PART, KICK, TOPIC
  | validateArray params 3 =
    -- JOIN, TOPIC, NICK, QUIT
    let server = getJSString $ params !! 0
        cmd    = B.map toLower (getJSString $ params !! 1)
        chan   = getJSString $ params !! 2
    in case cmd of 
         "join"    -> Right $ 
           MsgSend server (showCommand (MJoin chan Nothing)) numId
         "topic"   -> Right $ 
           MsgSend server (showCommand (MTopic chan Nothing)) numId
         "nick"    -> Right $ 
           MsgSend server (showCommand (MNick chan)) numId
         otherwise -> Left (numId, "Invalid command, got: " `B.append` cmd)
  | validateArray params 4 =
    -- PRIVMSG, PART, TOPIC, INVITE, NOTICE, ACTION
    let server = getJSString $ params !! 0
        cmd    = B.map toLower (getJSString $ params !! 1)
        chan   = getJSString $ params !! 2
        msg    = getJSString $ params !! 3
    in case cmd of 
         "privmsg" -> Right $
            MsgSend server (showCommand (MPrivmsg chan msg)) numId
         "part"    -> Right $
            MsgSend server (showCommand (MPart chan msg)) numId
         "topic"   -> Right $ 
            MsgSend server (showCommand (MTopic chan (Just msg))) numId
         "notice"  -> Right $
            MsgSend server (showCommand (MNotice chan msg)) numId
         "action"  -> Right $
            MsgSend server (showCommand (MAction chan msg)) numId
         otherwise -> Left (numId, "Invalid command, got: " `B.append` cmd)
  | validateArray params 5 =
    -- KICK
    let server = getJSString $ params !! 0
        cmd    = B.map toLower (getJSString $ params !! 1)
        chan   = getJSString $ params !! 2
        usr    = getJSString $ params !! 3
        msg    = getJSString $ params !! 4
    in case cmd of
         "kick"    -> Right $
            MsgSend server (showCommand (MKick chan usr msg)) numId
         otherwise -> Left (numId, "Invalid command, got: " `B.append` cmd)
    
  where numId = fromIntegral $ numerator id
  
rpcToSend (RPCRequest _ (JSArray params) Nothing) = 
  error "id is Nothing, expected something."

rpcToCmdAdd :: RPC -> Either (Int, B.ByteString) Message
rpcToCmdAdd (RPCRequest _ (JSArray params) (Just id)) = 
  Right $ MsgCmdAdd cmd (fromIntegral $ numerator id)
  where cmd = getJSString $ params !! 0

rpcToCmdAdd (RPCRequest _ (JSArray params) Nothing) = 
  error "id is Nothing, expected something."

rpcToPID :: RPC -> Either (Int, B.ByteString) Message
rpcToPID (RPCRequest _ (JSArray params) _) =
  Right $ MsgPid (read pid) -- TODO: Check whether it's an int.
  where pid = B.unpack $ getJSString $ params !! 0

decodeMessage :: String -> Either (Int, B.ByteString) Message
decodeMessage xs = rpcToMsg $ jsToRPC parsed
  where parsed = errorResult $ decode xs 
  
-- Writing JSON ----------------------------------------------------------------

showJSONMaybe :: (JSON t) => Maybe t -> JSValue
showJSONMaybe (Just a)  = showJSON a
showJSONMaybe (Nothing) = JSNull

showJSONIrcMessage :: IrcMessage -> JSValue
showJSONIrcMessage msg =
  JSObject $ toJSObject $
    [("nick", showJSONMaybe (mNick msg))
    ,("user", showJSONMaybe (mUser msg))
    ,("host", showJSONMaybe (mHost msg))
    ,("server", showJSONMaybe (mServer msg))
    ,("code", showJSON (mCode msg))
    ,("msg", showJSON (mMsg msg))
    ,("chan", showJSONMaybe (mChan msg))
    ,("other", showJSONMaybe (mOther msg))
    ,("raw", showJSON (mRaw msg))
    ]

showJSONMIrc :: MIrc -> IO JSValue
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

showJSONCommand :: PluginCommand -> IO JSValue
showJSONCommand (PCMessage msg serv) = do
  servJSON <- showJSONMIrc serv
  return $ JSObject $ toJSObject $
    [("method", showJSON ("recv" :: String))
    ,("params", JSArray [showJSONIrcMessage msg, servJSON])
    ,("id", JSNull)
    ]

showJSONCommand (PCCmdMsg msg serv prefix cmd) = do
  servJSON <- showJSONMIrc serv
  return $ JSObject $ toJSObject $
    [("method", showJSON ("cmd" :: String))
    ,("params", JSArray [showJSONIrcMessage msg, 
                         servJSON, showJSON prefix, showJSON cmd])
    ,("id", JSNull)
    ]

showJSONCommand (PCQuit) = do
  return $ JSObject $ toJSObject $
    [("method", showJSON ("quit" :: String))
    ,("params", JSArray [])
    ,("id", JSNull)
    ]

showJSONCommand (PCSuccess msg id) = do
  return $ JSObject $ toJSObject $
    [("result", showJSON msg)
    ,("error", JSNull)
    ,("id", showJSON id)
    ]

showJSONCommand (PCError err id) = do
  return $ JSObject $ toJSObject $
    [("result", showJSON ("error" :: String))
    ,("error", showJSON err)
    ,("id", showJSON id)
    ]

-- End of JSON -----------------------------------------------------------------

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
        let decoded = decodeMessage line

        case decoded of
          Right (MsgSend addr msg id) -> do 
            ret <- sendRawToServer mArgs addr msg
            if ret
              then writeCommand (PCSuccess "Message sent." id) mPlugin
              else writeCommand (PCError "Server doesn't exist." id) mPlugin
          Right (MsgPid pid)          -> do
            _ <- swapMVar mPlugin (plugin {pPid = Just pid}) 
            return ()
          Right (MsgCmdAdd cmd id)    -> do 
            _ <- swapMVar mPlugin (plugin {pCmds = (B.unpack cmd):pCmds plugin}) 
            writeCommand (PCSuccess "Command added." id) mPlugin
          Left  (id, err)             -> do
            writeCommand (PCError err id) mPlugin
            
      
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

sendRawToServer :: MVar MessageArgs -> B.ByteString -> B.ByteString -> IO Bool
sendRawToServer mArgs server msg = do
  args <- readMVar mArgs 
  servers <- readMVar $ argServers args
  filtered <- filterM (\srv -> do addr <- getAddress srv
                                  return $ addr == server) 
                         servers
  if not $ null filtered
    then do sendRaw (filtered !! 0) msg
            return True
    else return False

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




