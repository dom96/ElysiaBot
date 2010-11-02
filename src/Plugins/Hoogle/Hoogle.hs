{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages

import qualified Data.ByteString.Char8 as B
import Data.Maybe

import System.Process
import System.IO

main = do
  initPlugin ["hoogle"] [] onHoogle
  
onHoogle mInfo (MsgCmd cMsg server prefix cmd rest) = do
  evalResult <- findM (B.unpack rest)
  either (\err -> sendMsg $ "Error: " `B.append` (B.pack err))
         (\res -> mapM_ sendMsg (B.lines $ B.pack $ limitLines 200 4 res) )
         evalResult
  where addr = address server
        msg = mMsg cMsg
        sendMsg m = sendPrivmsg addr (fromJust $ mChan cMsg) m
onHoogle _ _ = return ()  

  
findM code = do
  putStrLn $ "Executing...\n  hoogle " ++ (escape [] code)
  (inpH, outH, errH, pid) <- runInteractiveProcess "hoogle" (words $ escape [] code) Nothing Nothing
  waitForProcess pid
  ret <- hGetContents outH
  err <- hGetContents errH
  putStrLn $ "Got contents...\n  " ++ ret
  putStrLn $ "Got error...\n " ++ err
  
  if null err
    then return $ Right ret
    else return $ Left $ limitMsg 200 err

limitLines lenLimit lineLimit xs =
   unlines $ map (limitMsg lenLimit) $ take lineLimit (lines xs)

limitMsg limit xs = 
  if length xs > limit
    then take limit xs ++ "..."
    else xs

escape :: String -> String -> String
escape "-" ('>':rest) = '#':(escape ">" rest)

escape _ (cur:rest)   = cur:(escape [cur] rest)
escape _ xs           = xs    

