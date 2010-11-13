{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  initPlugin ["testPlugin"] [] recvMsg

recvMsg mInfo (MsgCmd msg server prefix cmd _) = do
  let serv = (address server)
      origin = (fromJust $ mOrigin msg) 
  
  sendPrivmsg serv origin "Plugin works!!!"
  sendPrivmsg serv origin (mMsg msg)
  
recvMsg _ _ = do
  return ()
