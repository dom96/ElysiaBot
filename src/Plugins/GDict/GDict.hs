{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages

import qualified Data.ByteString.Char8 as B

import Data.Maybe (fromJust)

import GDictParse

main = do
  initPlugin ["dict"] [] onDict
  
onDict mInfo (MsgCmd cMsg server prefix cmd rest) = do
  evalResult <- lookupDict (B.unpack rest)
  either (\err -> sendMsg $ (B.pack err))
         (\res -> sendMsg $ B.pack $ limitMsg 200 (formatParsed res))
         evalResult
  where addr      = address server
        msg       = mMsg cMsg
        sendMsg m = sendPrivmsg addr (fromJust $ mChan cMsg) m
onDict _ _ = return ()

limitMsg limit xs = 
  if length xs > limit
    then take limit xs ++ "..."
    else xs

formatParsed :: ParsedDict -> String
formatParsed dict
  | not $ null $ related dict =
    (related dict !! 0) ++ " - " ++ (meanings dict !! 0)
  | otherwise = (meanings dict !! 0)
