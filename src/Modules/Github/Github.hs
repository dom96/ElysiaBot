{-# LANGUAGE OverloadedStrings #-}
module Modules.Github.Github (moduleCmds, moduleRaws, onLoad) where
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Control.Concurrent
import Network
import System.IO
import Network.HTTP.Headers
import Network.HTTP.Base
import Data.List
import Control.Monad

moduleCmds = M.empty

moduleRaws = M.empty

getBody2 :: [String] -> String
getBody2 ("\r":body) = unlines body
getBody2 (_:xs)      = getBody2 xs

getAllContents :: Handle -> IO String
getAllContents h = do
  eof <- hIsEOF h
  if not eof
    then do c <- hGetLine h
            c1 <- getAllContents h
            return $ c ++ "\n" ++ c1
    else return ""

getMethod :: Handle -> IO String
getMethod h = do
  eof <- hIsEOF h
  if not eof
    then hGetLine h
    else return ""

serverReply :: Handle -> String -> IO Bool
serverReply h method
  | "POST " `isPrefixOf` method = do
    hPutStrLn h "HTTP/1.0 200 OK"
    hPutStrLn h "Content-Length: 0"
    hPutStrLn h "Server: ElysiaBot"
    hPutStrLn h ""
    return True
  | method == "" = return False
  | otherwise    = do
    hPutStrLn h "HTTP/1.0 405 Method Not Allowed"
    -- I have absolutely no idea what that '+ 1' is needed for.
    hPutStrLn h ("Content-Length: " ++ (show $ length errBody + 1))
    hPutStrLn h "Content-Type: text/html"
    hPutStrLn h "Server: ElysiaBot"
    hPutStrLn h ""
    hPutStrLn h errBody
    return False
  where errBody = 
          "<p>OMG LOOK IT'S A TEAPOT!</p>\n" ++
          "<img src=\"http://jamorama.com/blog/wp-content/uploads/2009/10/teapot6bk1.jpg\"/>\n"

listenLoop s = do
  (h, hn, port) <- accept s
  hSetBuffering h LineBuffering
  putStrLn $ "Got connection from " ++ hn

  method <- getMethod h
  
  putStrLn $ show $ "POST " `isPrefixOf` method
  putStrLn $ "Method =: " ++ method
  
  correctReq <- serverReply h method
  
  if correctReq 
    then do contents <- getAllContents h
            let body = getBody2 $ lines contents

            putStrLn $ urlDecode body
            hClose h
    else hClose h
  
  listenLoop s

onLoad :: IO ()
onLoad = do 
  s <- listenOn (PortNumber 3456)
  forkIO (listenLoop s)
  putStrLn "Listening on 3456 for github connections."
