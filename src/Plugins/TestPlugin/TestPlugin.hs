{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  sendPID
  sendCmdAdd "testPlugin"
  pluginLoop recvMsg

{-
recvMsg :: Message -> IO ()
recvMsg rcvMsg = do
  --putStrLn $ "TestPlugin: " ++ show rcvMsg
  
  
  
  {-
  if mCode (ircMessage rcvMsg) == "PRIVMSG" && mMsg (ircMessage rcvMsg) == "|testPlugin"
    then sendRawMsg (B.unpack $ address $ ircServer rcvMsg) $
            "PRIVMSG " ++ (B.unpack $ fromJust $ mChan $ ircMessage rcvMsg) ++ " :" ++ "PLlugin works!!!"
    else return ()
  -}
-}

recvMsg (MsgCmd msg server prefix cmd) = do
  sendPrivmsg (B.unpack $ address server) (B.unpack $ fromJust $ mChan msg) "Plugin works!!!"
  sendPrivmsg (B.unpack $ address server) (B.unpack $ fromJust $ mChan msg) (B.unpack $ mMsg msg)
recvMsg _ = do
  return ()
