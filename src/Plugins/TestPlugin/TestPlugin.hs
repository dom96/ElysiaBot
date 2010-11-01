{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages
import qualified Data.ByteString.Char8 as B
import Data.Maybe
main = do
  initPlugin ["testPlugin"] [] recvMsg

recvMsg mInfo (MsgCmd msg server prefix cmd) = do
  let serv = (address server)
      chan = (fromJust $ mChan msg) 
  
  sendPrivmsg serv chan "Plugin works!!!"
  sendPrivmsg serv chan (mMsg msg)
  
recvMsg _ _ = do
  return ()
