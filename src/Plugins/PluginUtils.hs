{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, StandaloneDeriving #-}
module PluginUtils (Message(..), decodeMessage, pluginLoop, sendPID) where
import Network.SimpleIRC

import Text.JSON
import Text.JSON.String
import Text.JSON.Generic

import Data.List

import System.Posix.Process (getProcessID)

import qualified Data.ByteString.Char8 as B

data MServer = MServer
  { sAddress  :: B.ByteString
  , sNickname :: B.ByteString
  , sUsername :: B.ByteString
  , sChans    :: [B.ByteString]
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
      "recv" -> 
        let (Ok iMsg) = fromJSON (snd $ objects !! 1) :: Result IrcMessage
            (Ok iSrv) = fromJSON (snd $ objects !! 2) :: Result MServer
        in  Ok $ MsgRecv iMsg iSrv
      otherwise -> Error $ "Invalid type, got " ++ theType
    where objects = fromJSObject jsonObjects
          theType = (readType $ objects !! 0) 
  showJSON = undefined
  
readType ("type", (JSString theType)) = fromJSString theType
readType (name, _) = error $ "Couldn't find 'type' or type is not a string, got " ++ name

{-
instance JSON IrcMessage where
  readJSON (JSObject jsonObjects) =
    Ok $ foldl' (flip readIrcMessageJSON) (defaultIrcMessage) objects
    where objects = fromJSObject jsonObjects
  showJSON = undefined

readIrcMessageJSON :: (String, JSValue) -> IrcMessage -> IrcMessage
readIrcMessageJSON ("nick", (JSString nick)) iMsg = 
  iMsg {mNick = Just $ B.pack $ fromJSString nick}
readIrcMessageJSON ("nick", (JSNull)) iMsg = 
  iMsg {mNick = Nothing}
readIrcMessageJSON ("user", (JSString user)) iMsg = 
  iMsg {mUser = Just $ B.pack $ fromJSString user}
readIrcMessageJSON ("user", (JSNull)) iMsg = 
  iMsg {mUser = Nothing}
readIrcMessageJSON ("host", (JSString host)) iMsg = 
  iMsg {mHost = Just $ B.pack $ fromJSString host}
readIrcMessageJSON ("host", (JSNull)) iMsg = 
  iMsg {mHost = Nothing}
readIrcMessageJSON ("server", (JSString server)) iMsg = 
  iMsg {mServer = Just $ B.pack $ fromJSString server}
readIrcMessageJSON ("server", (JSNull)) iMsg = 
  iMsg {mServer = Nothing}
readIrcMessageJSON ("code", (JSString code)) iMsg = 
  iMsg {mCode = B.pack $ fromJSString code}
readIrcMessageJSON ("msg", (JSString msg)) iMsg = 
  iMsg {mMsg = B.pack $ fromJSString msg}
readIrcMessageJSON ("chan", (JSString chan)) iMsg = 
  iMsg {mChan = Just $ B.pack $ fromJSString chan}
readIrcMessageJSON ("chan", (JSNull)) iMsg = 
  iMsg {mChan = Nothing}
readIrcMessageJSON ("other", (JSArray other)) iMsg = 
  iMsg {mOther = Just $ fromJSArrayString other}
readIrcMessageJSON ("other", JSNull) iMsg = 
  iMsg {mOther = Nothing}
readIrcMessageJSON ("raw", (JSString raw)) iMsg = 
  iMsg {mRaw = B.pack $ fromJSString raw}

instance JSON MServer where
  readJSON (JSObject jsonObjects) =
    Ok $ foldl' (flip readMServerJSON) (defaultMServer) objects
    where objects = fromJSObject jsonObjects
  showJSON = undefined

readMServerJSON :: (String, JSValue) -> MServer -> MServer
readMServerJSON ("address", (JSString address)) iSrv = 
  iSrv {sAddress = B.pack $ fromJSString address}
readMServerJSON ("nickname", (JSString nick)) iSrv = 
  iSrv {sNickname = B.pack $ fromJSString nick}
readMServerJSON ("username", (JSString user)) iSrv = 
  iSrv {sUsername = B.pack $ fromJSString user}
readMServerJSON ("chans", (JSArray chans)) iSrv = 
  iSrv {sChans = fromJSArrayString chans}

fromJSArrayString :: [JSValue] -> [B.ByteString]
fromJSArrayString strings =
  map str strings
  where str (JSString s) = B.pack $ fromJSString s

fromOk :: Result a -> a
fromOk (Ok a) = a
fromOk (Error a) = error a 
-}
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
  pid <- getProcessID
  putStrLn $ "{ \"type\": \"pid\", \"pid\": " ++ (show pid) ++ " }"
   
