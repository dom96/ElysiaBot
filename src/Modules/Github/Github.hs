{-# LANGUAGE OverloadedStrings #-}
module Modules.Github.Github (moduleCmds, moduleRaws, onLoad, ) where
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.MVar
import Network
import System.IO
import Network.HTTP.Headers
import Network.HTTP.Base
import Data.List
import Control.Monad
import Modules.Github.ParsePayload

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

serverReply :: Handle -> String -> IO (Maybe (String, String))
serverReply h method
  | "POST " `isPrefixOf` method = do
    hPutStrLn h "HTTP/1.0 200 OK"
    hPutStrLn h "Content-Length: 0"
    hPutStrLn h "Server: ElysiaBot"
    hPutStrLn h ""
    
    let (addr, chan) = break (== '/') (words (drop 6 method) !! 0)
    return $ Just (addr, "#" ++ (drop 1 chan))

  | method == "" = return Nothing
  | otherwise    = do
    hPutStrLn h "HTTP/1.0 405 Method Not Allowed"
    -- I have absolutely no idea what that '+ 1' is needed for.
    hPutStrLn h ("Content-Length: " ++ (show $ length errBody + 1))
    hPutStrLn h "Content-Type: text/html"
    hPutStrLn h "Server: ElysiaBot"
    hPutStrLn h ""
    hPutStrLn h errBody
    return Nothing
  where errBody = 
          "<p>OMG LOOK IT'S A TEAPOT!</p>\n" ++
          "<img src=\"http://jamorama.com/blog/wp-content/uploads/2009/10/teapot6bk1.jpg\"/>\n"

announce :: String -> String -> [MIrc] -> String -> IO ()
announce servAddr chan servers msg = do
  forM_ servers (\s -> do
    addr <- getAddress s
    when ((B.unpack addr) == servAddr)
         (sendMsg s (B.pack chan) (B.pack msg)))

listenLoop s serversM = do
  (h, hn, port) <- accept s
  hSetBuffering h LineBuffering
  putStrLn $ "Got connection from " ++ hn

  method <- getMethod h
  
  putStrLn $ show $ "POST " `isPrefixOf` method
  putStrLn $ "Method =: " ++ method
  
  correctReq <- serverReply h method
  servers <- readMVar serversM
  
  if isJust $ correctReq 
    then do let (addr, chan) = fromJust correctReq
            contents <- getAllContents h
            let body   = urlDecode $ getBody2 $ lines contents
                parsed = parseAll body
            putStrLn $ "Chan = " ++ chan ++ " Addr = " ++ addr
            either (\e -> putStrLn $ "ParseError! - " ++ e)
                   (\p -> announce addr chan servers (formatOutput p))
                   parsed
            
            hClose h
    else hClose h
  
  listenLoop s serversM

-- dom96/SimpleIRC - 3 commits on refs/heads/master.
-- dom96: +[whatever.hs, this.hs... 5] -[blah.hs] +-[that.hs] Message
-- dom96: +-[that.hs] Message

formatOutput :: Payload -> String
formatOutput payload = 
  "\x02" ++ (a_name (owner $ repository payload)) ++ "/" ++
  (name $ repository payload) ++ "\x02 - " ++
  (show $ length (commits payload)) ++ " commits on " ++
  (formatRef $ ref payload) ++ ".\n" ++
  (unlines $ take 3 (map formatCommit (commits payload)))

formatRef ref
  | "refs/heads/" `isPrefixOf` ref =
    drop 11 ref
  | otherwise = ref

formatAddRemMod which xs
  | not $ null xs = 
    which ++ show xs ++ " "
  | otherwise        = ""

formatCommit commit =
  authorName ++ (formatAddRemMod "\x02+\x02" (added commit)) ++
  (formatAddRemMod "\x02-\x02" (removed commit)) ++
  (formatAddRemMod "\x02+-\x02" (modified commit)) ++ (message commit)

  where authorName = "\x02" ++ (a_name (author commit)) ++ ":\x02 "

onLoad :: MVar [MIrc] -> IO ()
onLoad serversM = do 
  s <- listenOn (PortNumber 3456)
  forkIO (listenLoop s serversM)
  putStrLn "Listening on 3456 for github connections."
