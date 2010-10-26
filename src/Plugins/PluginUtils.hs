{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, StandaloneDeriving #-}
module PluginUtils (Message(..), MServer(..), decodeMessage, pluginLoop, sendPID, sendCmdAdd, sendRawMsg, sendPrivmsg) where
import System.IO
import System.Posix.Process (getProcessID)

import Network.SimpleIRC

import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types

import Data.Maybe

import qualified Data.ByteString.Char8 as B

deriving instance Data IrcMessage

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
    } 
  | MsgQuit
  deriving (Typeable, Data, Show)

validateFields :: JSValue -> [String] -> Bool
validateFields (JSObject obj) fields =
  let exist = map (get_field obj) fields
  in all (isJust) exist

-- -.-
getJSString :: JSValue -> String
getJSString (JSString (JSONString s)) = s

getJSMaybe :: JSValue -> Maybe JSValue
getJSMaybe (JSObject obj) = 
  get_field obj "Just"
getJSMaybe (JSString (JSONString s)) = 
  if s == "Nothing"
    then Nothing
    else error $ "Maybe in a JSON literal is a string, but it is not, \"Nothing\", got " ++ s

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
         { reqMethod = B.pack $ getJSString $ fromJust $ get_field obj "method"
         , reqParams = fromJust $ get_field obj "params" 
         , reqId     = if isJust $ rID 
                          then Just $ getJSRatio $ fromJust rID
                          else Nothing 
         }

  -- TODO: RPCResponse.

-- This function just checks the reqMethod of RPCRequest.
rpcToMsg :: RPC -> Message
rpcToMsg req@(RPCRequest method _ _)
  | method == "recv" = rpcToRecv req
  | method == "cmd"  = rpcToCmd  req

-- Turns an RPC(Which must be a RPCRequest with a method of "recv") into a MsgRecv.
rpcToRecv :: RPC -> Message
rpcToRecv (RPCRequest _ (JSArray params) _) = 
  MsgRecv (errorResult (fromJSON msg :: Result IrcMessage))
          (errorResult (fromJSON server :: Result MServer))
  where msg    = params !! 0
        server = params !! 1

rpcToCmd :: RPC -> Message
rpcToCmd (RPCRequest _ (JSArray params) _) = 
  MsgCmd (errorResult (fromJSON msg :: Result IrcMessage))
         (errorResult (fromJSON server :: Result MServer))
         prfx command
  where msg     = params !! 0
        server  = params !! 1
        prfx    = B.pack $ getJSString $ params !! 2
        command = B.pack $ getJSString $ params !! 3
        
decodeMessage :: String -> Message
decodeMessage xs = rpcToMsg $ jsToRPC parsed
  where parsed = errorResult $ decode xs 

-- End of JSON -----------------------------------------------------------------

pluginLoop :: (Message -> IO ()) -> IO ()
pluginLoop func = do
  line <- getLine
  let msg = decodeMessage line
  func msg
  pluginLoop func

sendPID :: IO ()
sendPID = do
  hSetBuffering stdout LineBuffering -- Without this, stdout isn't flushed every putStrLn
  pid <- getProcessID
  putStrLn $ "{ \"method\": \"pid\", \"params\": [ \"" ++ (show pid) ++ "\" ], \"id\": \"Nothing\" }"
  --hFlush stdout

sendCmdAdd cmd = do
  putStrLn $ "{ \"method\": \"cmdadd\", \"params\": [ \"" ++ cmd ++ "\" ], \"id\": 0 }" -- TODO: Change ID ?

sendRawMsg :: String -> String -> IO ()
sendRawMsg server msg = do
  let m = "{ \"method\": \"send\", \"params\": [\"" ++ server ++ "\", \"" ++ msg ++ "\"], \"id\": 0 }"
  putStrLn $ m
  --hFlush stdout
  
sendPrivmsg :: String -> String -> String -> IO ()
sendPrivmsg server chan msg = do
  sendRawMsg server ("PRIVMSG " ++ chan ++ " :" ++ msg)

