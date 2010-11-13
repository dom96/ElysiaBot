{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages

import qualified Data.ByteString.Char8 as B

import Data.Maybe

import System.Process
import System.IO

import Control.Monad (when)

main = do
  initPlugin ["heval"] ["privmsg"] onMessage
  
onMessage mInfo (MsgCmd cMsg server prefix cmd rest) = do
  evalResult <- evalM rest
  either (\err -> mapM_ sendMsg (B.lines $ "Error: " `B.append` err) )
         (\(_, _, res) -> mapM_ (sendMsg . (B.append "=> ")) 
                                (B.lines $ limitMsg 200 res))
         evalResult
  where addr = address server
        msg = mMsg cMsg
        sendMsg m = sendPrivmsg addr (fromJust $ mOrigin cMsg) m
onMessage mInfo (MsgRecv msg server) = do
  when ("> " `B.isPrefixOf` (mMsg msg) ) $
    onMessage mInfo (MsgCmd msg server ">" " " (B.drop 2 (mMsg msg)))

onMessage mInfo _ = return ()

evalM :: B.ByteString -> IO (Either B.ByteString (B.ByteString, B.ByteString, B.ByteString))
evalM code = do
  B.putStrLn $ "Executing...\n  mueval -i --expression \"" 
               `B.append` (escapeCode code) `B.append` "\""
  (inpH, outH, errH, pid) <- runInteractiveProcess "mueval" 
                             ["-i","--expression", B.unpack code] 
                             Nothing Nothing
  waitForProcess pid
  ret <- B.hGetContents outH
  err <- B.hGetContents errH
  B.putStrLn $ "Got contents...\n  " `B.append` ret
  B.putStrLn $ "Got error...\n " `B.append` err
  
  if B.null err
    then return $ parseRet ret code   
    else return $ Left $ limitMsg 200 err

formatErr :: B.ByteString -> B.ByteString
formatErr err
  | length e > 1 = 
    let second = (limitMsg 28 $ e !! 1)
    in first `B.append` "\n" `B.append` second
  | otherwise = first
  where e = take 2 $ B.lines err
        first  = (limitMsg 350 $ e !! 0) 

limitMsg :: Int -> B.ByteString -> B.ByteString
limitMsg limit xs = 
  if B.length xs > limit
    then B.take limit xs `B.append` "..."
    else xs

parseRet :: B.ByteString -> B.ByteString -> Either B.ByteString (B.ByteString, B.ByteString, B.ByteString)
parseRet ret code 
  | (B.lines ret !! 0) /= code  = Left $ formatErr ret
  | (length $ B.lines ret) > 2  =
    let (expr, typ, result) = (B.lines ret !! 0, B.lines ret !! 1, B.lines ret !! 2)
    in Right (expr, typ, result)
  | otherwise                 = Left $ "Unknown error. Received " `B.append` ret

escapeCode :: B.ByteString -> B.ByteString
escapeCode code = 
  B.concatMap (\c -> case c of '\"' -> "\\\""
                               '`' -> "\\`"
                               otherwise -> B.pack [c]) code
