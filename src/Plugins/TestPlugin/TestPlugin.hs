{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  initPlugin ["testPlugin"] recvMsg

recvMsg mInfo (MsgCmd msg server prefix cmd) = do
  let serv = (B.unpack $ address server)
      chan = (B.unpack $ fromJust $ mChan msg) 
  
  sendPrivmsg serv chan "Plugin works!!!"
  sendPrivmsg serv chan (B.unpack $ mMsg msg)
  
recvMsg _ _ = do
  return ()
