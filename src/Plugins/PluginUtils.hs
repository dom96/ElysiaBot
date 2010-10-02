{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, StandaloneDeriving #-}
module PluginUtils (Message(..), MServer(..), decodeMessage, pluginLoop, sendPID, sendRawMsg) where
import Network.SimpleIRC

import Text.JSON
import Text.JSON.String
import Text.JSON.Generic

import Data.List

import System.Posix.Process (getProcessID)
import System.IO

import qualified Data.ByteString.Char8 as B

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
  | MsgQuit
  deriving (Typeable, Data, Show)

deriving instance Data IrcMessage

instance JSON Message where
  readJSON (JSObject jsonObjects) = 
    case theType of 
      "quit" -> Ok MsgQuit
      "recv" -> readRecv objects
      otherwise -> Error $ "Invalid type, got " ++ theType
    where objects = fromJSObject jsonObjects
          theType = (readType $ objects !! 0) 
  showJSON = undefined
  
readType ("type", (JSString theType)) = fromJSString theType
readType (name, _) = error $ "Couldn't find 'type' or type is not a string, got " ++ name

readRecv objects =
  if fst frstObj == "IrcMessage" && fst sndObj == "IrcServer"
    then let (Ok iMsg) = fromJSON (snd $ objects !! 1) :: Result IrcMessage
             (Ok iSrv) = fromJSON (snd $ objects !! 2) :: Result MServer
         in   Ok $ MsgRecv iMsg iSrv
    else Error "Invalid objects"
  where frstObj = objects !! 1
        sndObj = objects !! 2


decodeMessage :: String -> Message
decodeMessage st = let takeOk (Ok decoded) = decoded
                       takeOk (Error err)  = error err
                   in takeOk (decode st :: Result Message)

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
  putStrLn $ "{ \"type\": \"pid\", \"pid\": \"" ++ (show pid) ++ "\" }"
  --hFlush stdout

sendRawMsg :: String -> String -> IO ()
sendRawMsg server msg = do
  let m = "{ \"type\": \"send\", \"server\": \"" ++ server ++ "\", \"msg\": \"" ++ msg ++ "\" }"
  putStrLn $ m
  --hFlush stdout
