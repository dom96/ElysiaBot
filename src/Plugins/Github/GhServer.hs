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
import Control.Applicative
import ParsePayload

allowedUsers = ["dom96", "XAMPP", "Amrykid", "RockerMONO"]

shortenURL addr = do
  (url, rsp) <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               setOutHandler (\a -> return ())
               request $ getRequest ("http://is.gd/api.php?longurl=" ++ urlEncode addr)
  if rspCode rsp == (2,0,0)
    then return $ rspBody rsp
    else return $ rspBody rsp

getAllLines :: Handle -> IO [String]
getAllLines h = liftA2 (:) first rest `catch` (\err -> return []) 
  where first = hGetLine h
        rest = getAllLines h

sendOK :: Handle -> IO ()
sendOK h = do
  hPutStrLn h "HTTP/1.1 200 OK"
  hPutStrLn h "Content-Length: 0"
  hPutStrLn h "Server: ElysiaBot"
  hPutStrLn h ""
  putStrLn "Replied with 200 OK."

sendError :: Handle -> IO ()
sendError h = do
  hPutStrLn h "HTTP/1.1 405 Method Not Allowed"
  -- I have absolutely no idea what that '+ 1' is needed for.
  hPutStrLn h ("Content-Length: " ++ (show $ length errBody + 1))
  hPutStrLn h "Content-Type: text/html"
  hPutStrLn h "Server: ElysiaBot"
  hPutStrLn h "Connection: Close"
  hPutStrLn h ""
  hPutStrLn h errBody
  
  putStrLn "Replied with error message."
  where errBody = 
          "<p>OMG LOOK IT'S A TEAPOT!</p>\n" ++
          "<img src=\"http://jamorama.com/blog/wp-content/uploads/2009/10/teapot6bk1.jpg\"/>\n"

getInfo :: String -> Maybe (String, String)
getInfo method
  | "POST " `isPrefixOf` method =
    let (addr, chan) = break (== '/') (words (drop 6 method) !! 0)
    in Just (addr, "#" ++ (urlDecode $ drop 1 chan))
  | otherwise = Nothing
  
sendContinue :: Handle -> String -> IO ()
sendContinue h headerValue
  | "100" `isPrefixOf` headerValue = do
    hPutStrLn h "HTTP/1.1 100 Continue"
    hPutStrLn h ""
    putStrLn $ "Replied with 100 Continue."
  | otherwise =
    putStrLn $ "Unexpected 'Expect' header, got: " ++ headerValue

replyHeader :: Handle -> Header -> IO ()
replyHeader h he
  | hdrName he == HdrExpect = do
    sendContinue h (hdrValue he)
  | otherwise = return ()

safeHGetLine :: Handle -> IO (Maybe String)
safeHGetLine h = liftA (\a -> Just a) line `catch` (\_ -> return Nothing)
  where line = hGetLine h

maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing  = ""

goHeaders :: Handle -> IO ()
goHeaders h = do
  line <- safeHGetLine h
  if isJust line
    then do if (fromJust line) == "\r"
              then return ()
              else do let parsedHeader = parseHeader (fromJust line)
                      either (\err -> putStrLn $ show err)
                             (replyHeader h)
                             parsedHeader
                      goHeaders h
    else putStrLn "hGetLine failed."

listenLoop s = do
  (h, hn, port) <- accept s
  hSetBuffering h LineBuffering
  putStrLn $ "Got connection from " ++ hn

  method <- safeHGetLine h
  
  putStrLn $ "Method =: " ++ (maybeToString method)
  
  let correctReq = getInfo (maybeToString method)
  
  if (isJust correctReq) && ("github.com" `isSuffixOf` hn || hn == "localhost")
    then do let (addr, chan) = fromJust correctReq
            goHeaders h -- Check if a Expect header is present.
            sendOK h -- Send 200 OK.
            
            -- Get the JSON body.
            body <- getAllLines h 
            let parsedBody = parseAll (urlDecode $ unlines body)

            putStrLn $ "Chan = " ++ chan ++ " Addr = " ++ addr
            either (\e -> putStrLn $ "ParseError! - " ++ e)
                   (\p -> do 
                     let allowed = any (== (a_name (owner $ repository p))) allowedUsers
                     when allowed $ do
                       formatted <- formatOutput p
                       sendPrivmsg (B.pack addr) (B.pack chan) (B.pack formatted))
                   parsedBody
            
            hClose h
    else do sendError h
            hClose h
  
  listenLoop s

-- curl -v localhost:3456/irc.freenode.net/HSBotTest -d @gh.txt
            
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
