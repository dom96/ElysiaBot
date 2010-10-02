{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  sendPID
  pluginLoop recvMsg

recvMsg :: Message -> IO ()
recvMsg rcvMsg = do
  --putStrLn $ "TestPlugin: " ++ show rcvMsg
  if mCode (ircMessage rcvMsg) == "PRIVMSG" && mMsg (ircMessage rcvMsg) == "|testPlugin"
    then sendRawMsg (B.unpack $ address $ ircServer rcvMsg) $
            "PRIVMSG " ++ (B.unpack $ fromJust $ mServer $ ircMessage rcvMsg) ++ " :" ++ "PLlugin works!!!"
    else return ()
