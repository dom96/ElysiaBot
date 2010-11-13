{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module PluginUtils 
  ( Message(..)
  , MServer(..)
  , MInfo
  , RecvFunc
  , decodeMessage
  , initPlugin
  , pluginLoop
  , awaitResponse
  , success
  , sendPID
  , sendCmdAdd
  , sendIrcAdd
  , sendRawMsg
  , sendPrivmsg
  ) where

import System.IO
import System.Exit
import System.Posix.Process (getProcessID)

import Network.SimpleIRC

import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Data.Maybe
import Data.Ratio (numerator)

import Control.Monad(liftM)

import Control.Concurrent.MVar

import qualified Data.ByteString.Char8 as B

data RPC = 
    RPCRequest
      { reqMethod :: B.ByteString
      , reqParams :: JSValue
      , reqId     :: Maybe Integer
      } 
  | RPCResponse 
      { rspResult :: B.ByteString
      , rspError  :: Maybe B.ByteString
      , rspId     :: Integer
      }
  deriving (Typeable, Show)

data MServer = MServer
  { address  :: B.ByteString
  , nickname :: B.ByteString
  , username :: B.ByteString
  , chans    :: [B.ByteString]
  } deriving (Typeable, Data, Show)

data Message = 
   MsgRecv 
    { ircMessage :: IrcMessage
    , ircServer  :: MServer
    } 
  | MsgCmd 
    { ircMessage :: IrcMessage
    , ircServer  :: MServer
    , prefix     :: B.ByteString
    , cmd        :: B.ByteString
    , rest       :: B.ByteString
    } 
  | MsgQuit
  | MsgSuccess
    { msg :: B.ByteString
    , sId :: Int
    }
  | MsgError
    { err :: B.ByteString
    , eId :: Int
    }
  deriving (Typeable, Show)

type MInfo = MVar [B.ByteString]
type RecvFunc = (MInfo -> Message -> IO ())

validateFields :: JSValue -> [String] -> Bool
validateFields (JSObject obj) fields =
  let exist = map (get_field obj) fields
  in all (isJust) exist

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

getJSArray :: JSValue -> [JSValue]
getJSArray (JSArray arr) = arr

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
         , reqId     = if isJust rID 
                          then Just $ numerator $ getJSRatio $ fromJust rID
                          else Nothing 
         }

  | validateFields js ["result", "error", "id"] =
    let rErr = getJSMaybe $ fromJust $ get_field obj "error" 
    in RPCResponse
         { rspResult = getJSString $ fromJust $ get_field obj "result"
         , rspError  = if isJust rErr
                         then Just $ getJSString $ fromJust rErr
                         else Nothing
         , rspId     = numerator $ getJSRatio $ fromJust $ get_field obj "id"
         }

-- This function just checks the reqMethod of RPCRequest.
rpcToMsg :: RPC -> Message
rpcToMsg req@(RPCRequest method _ _)
  | method == "recv" = rpcToRecv req
  | method == "cmd"  = rpcToCmd  req
  | method == "quit" = MsgQuit
rpcToMsg rsp@(RPCResponse _ (Just _) _) = rpcToError   rsp
rpcToMsg rsp@(RPCResponse _ Nothing _)  = rpcToSuccess rsp

-- Turns an RPC(Which must be a RPCRequest with a method of "recv") into a MsgRecv.
rpcToRecv :: RPC -> Message
rpcToRecv (RPCRequest _ (JSArray params) _) = 
  MsgRecv (readJSONIrcMessage msg)
          (errorResult (fromJSON server :: Result MServer))
  where msg    = params !! 0
        server = params !! 1

rpcToCmd :: RPC -> Message
rpcToCmd (RPCRequest _ (JSArray params) _) = 
  MsgCmd (readJSONIrcMessage msg)
         (errorResult (fromJSON server :: Result MServer))
         prfx command rest
  where msg     = params !! 0
        server  = params !! 1
        prfx    = getJSString $ params !! 2
        command = getJSString $ params !! 3
        rest    = getJSString $ params !! 4

rpcToSuccess :: RPC -> Message
rpcToSuccess (RPCResponse result _ id) =
  MsgSuccess result (fromIntegral $ id)

rpcToError :: RPC -> Message
rpcToError (RPCResponse _ (Just err) id) =
  MsgError err (fromIntegral $ id)

decodeMessage :: B.ByteString -> Message
decodeMessage xs = rpcToMsg $ jsToRPC parsed
  where parsed = errorResult $ decode (B.unpack xs) 

readJSONIrcMessage :: JSValue -> IrcMessage
readJSONIrcMessage js@(JSObject obj)
  | validateFields js ["nick", "user", "host", 
      "server", "code", "msg", "chan", "other", "raw"] =
    IrcMessage
      { mNick = readJSONMaybe getJSString 
                  (getJSMaybe $ fromJust $ get_field obj "nick")
      , mUser = readJSONMaybe getJSString 
                  (getJSMaybe $ fromJust $ get_field obj "user")
      , mHost = readJSONMaybe getJSString 
                  (getJSMaybe $ fromJust $ get_field obj "host")
      , mServer = readJSONMaybe getJSString 
                  (getJSMaybe $ fromJust $ get_field obj "server")
      , mCode = getJSString $ fromJust $ get_field obj "code"
      , mMsg  = getJSString $ fromJust $ get_field obj "msg"
      , mChan = readJSONMaybe getJSString 
                  (getJSMaybe $ fromJust $ get_field obj "chan")
      , mOther = readJSONMaybe (\a -> map (getJSString) (getJSArray a)) 
                    (getJSMaybe $ fromJust $ get_field obj "other")
      , mRaw  = getJSString $ fromJust $ get_field obj "raw"
      }
  | otherwise = error "Invalid fields for IrcMessage."
    

readJSONMaybe :: JSON t => (JSValue -> t) -> Maybe JSValue -> Maybe t
readJSONMaybe f (Just val) = Just $ f val
readJSONMaybe _ Nothing    = Nothing

-- End of reading JSON ---------------------------------------------------------

showJSONMaybe :: (JSON t) => Maybe t -> JSValue
showJSONMaybe (Just a)  = showJSON a
showJSONMaybe (Nothing) = JSNull

showJSONRPC :: RPC -> JSValue
showJSONRPC (RPCRequest method params id) =
  JSObject $ toJSObject $
    [("method", showJSON method)
    ,("params", params)
    ,("id", showJSONMaybe id)
    ]
showJSONRPC (RPCResponse result error id) =
  JSObject $ toJSObject $
    [("result", showJSON result)
    ,("error", showJSONMaybe error)
    ,("id", showJSON id)
    ]

-- End of writing JSON ---------------------------------------------------------

-- |Takes care of the initialization of the plugin, sending the PID, adding
-- |the commands/codes etc.
initPlugin :: [String] -> [String] -> RecvFunc -> IO ()
initPlugin commands codes func = do
  sendPID
  mapM_ (sendCmdAdd) commands
  mapM_ (sendIrcAdd) codes
  mInfo <- newMVar []
  pluginLoop mInfo func

-- MInfo stores any messages that haven't been parsed.
pluginLoop :: MInfo -> RecvFunc -> IO ()
pluginLoop mInfo func = do
  -- Check mInfo for any unparsed messages
  info <- readMVar mInfo
  if not $ null info
    then mapM_ (decodeExec func) info -- Parse the messages.
    else return ()
  
  line <- B.getLine
  decodeExec func line
  pluginLoop mInfo func
  
  where decodeExec func m = do 
          let msg = decodeMessage m
          func mInfo msg
          onQuit msg
        onQuit (MsgQuit) = do exitSuccess
        onQuit _         = return ()
isStatus :: Message -> Bool
isStatus (MsgSuccess _ _) = True
isStatus (MsgError   _ _) = True
isStatus _                = False

-- |Returns true if message is a MsgSuccess, and false if it's a MsgError
-- |raises an error otherwise.
success :: Message -> Bool
success (MsgSuccess _ _) = True
success (MsgError   _ _) = False
success _                = error "Expected either a MsgSuccess or MsgError."

-- |Waits for a response, either a MsgSuccess or MsgError.
awaitResponse :: MInfo -> IO Message
awaitResponse mInfo = do
  line <- B.getLine
  let msg = decodeMessage line
  if isStatus msg
    then return msg
    else do _ <- withMVar mInfo (\i -> return (line:i))
            awaitResponse mInfo

sendRPC :: RPC -> IO ()
sendRPC rpc = do
  let (JSObject json) = showJSONRPC rpc
  putStrLn $ (showJSObject json) ""

-- |Sends PID information about the current plugin, this function also
-- |changes the buffering of stdout.
sendPID :: IO ()
sendPID = do
  hSetBuffering stdout LineBuffering -- Without this, stdout isn't flushed every putStrLn
  pid <- getProcessID
  let req = RPCRequest "pid" (JSArray [(showJSON (show pid))]) Nothing
  
  sendRPC req
  --hFlush stdout

-- TODO: Make the ID's unique.

-- |Sends the 'cmdadd' command.
sendCmdAdd :: String -> IO ()
sendCmdAdd cmd = do
  let req = RPCRequest "cmdadd" (JSArray [(showJSON cmd)]) (Just 0)
  sendRPC req

-- |Sends the 'ircadd' command.
sendIrcAdd :: String -> IO ()
sendIrcAdd code = do
  let req = RPCRequest "ircadd" (JSArray [(showJSON code)]) (Just 0)
  sendRPC req

-- |Sends a raw IRC Message
sendRawMsg :: B.ByteString -> B.ByteString -> IO ()
sendRawMsg server msg = do
  let req = RPCRequest "send" (JSArray [(showJSON server), (showJSON msg)]) 
                       (Just 0)
  sendRPC req
  --hFlush stdout

-- |Sends a command, with params.
sendCommand :: B.ByteString -> B.ByteString -> [B.ByteString] -> IO ()
sendCommand server cmd params = do
  let p   = [(showJSON server), (showJSON cmd)] ++ 
            map showJSON params
      req = RPCRequest "send" (JSArray p) (Just 0)
  sendRPC req

-- |Sends a PRIVMSG command.
sendPrivmsg :: B.ByteString -> B.ByteString -> B.ByteString -> IO ()
sendPrivmsg server chan msg = do
  sendCommand server "PRIVMSG" [chan, msg]

