module GhServer (listenLoop) where
import PluginUtils
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.MVar
import Network
import System.IO
import Network.HTTP.Headers
import Network.HTTP.Base
import Network.HTTP
import Network.Browser
import Data.List
import Control.Monad
import ParsePayload

allowedUsers = ["dom96", "XAMPP", "Amrykid", "RockerMONO"]

shortenURL addr = do
  (url, rsp) <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest ("http://is.gd/api.php?longurl=" ++ urlEncode addr)
  if rspCode rsp == (2,0,0)
    then return $ rspBody rsp
    else return $ rspBody rsp

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
    return $ Just (addr, "#" ++ (urlDecode $ drop 1 chan))

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

listenLoop s = do
  (h, hn, port) <- accept s
  hSetBuffering h LineBuffering
  putStrLn $ "Got connection from " ++ hn

  method <- getMethod h
  
  putStrLn $ "Starts with POST? = " ++ (show $ "POST " `isPrefixOf` method)
  putStrLn $ "Method =: " ++ method
  
  correctReq <- serverReply h method
  
  if (isJust correctReq) && "github.com" `isSuffixOf` hn
    then do let (addr, chan) = fromJust correctReq
            contents <- getAllContents h
            let body   = urlDecode $ getBody2 $ lines contents
                parsed = parseAll body
            
            putStrLn $ "Chan = " ++ chan ++ " Addr = " ++ addr
            either (\e -> putStrLn $ "ParseError! - " ++ e)
                   (\p -> do 
                     let allowed = any (== (a_name (owner $ repository p))) allowedUsers
                     when allowed $ do
                       formatted <- formatOutput p
                       sendPrivmsg (B.pack addr) (B.pack chan) (B.pack formatted))
                   parsed
            
            hClose h
    else hClose h
  
  listenLoop s

-- dom96/SimpleIRC - 3 commit(s) on master, f3g45g.. <http://is.gd/whate> ..y54gsf
-- dom96: +[whatever.hs, this.hs... 5] -[blah.hs] +-[that.hs] Message
-- dom96: +-[that.hs] Message

formatOutput :: Payload -> IO String
formatOutput payload = do
  compareView <- shortenURL (p_compare payload)
  return $ -- Clean this up lol
    "\x02" ++ (a_name (owner $ repository payload)) ++ "/" ++
    (name $ repository payload) ++ "\x02 - " ++
    (show $ length (commits payload)) ++ " commit(s) on " ++
    (formatRef $ ref payload) ++ ", " ++ (take 6 (before payload)) ++ ".. <" ++
    compareView ++ "> .." ++ (take 6 (after payload)) ++ "\n" ++
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
