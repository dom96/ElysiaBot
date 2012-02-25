{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (lookup)
import PluginUtils
import Network.SimpleIRC.Messages

import qualified Data.ByteString.Char8 as B

import Network.AMI
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, empty, append)
import Data.List (intercalate)
import Data.Map (fromList, findWithDefault, lookup)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Char (toLower)

info :: ConnectInfo
info = ConnectInfo {
         ciHost = "asterisk.example.net"
       , ciPort = 5038
       , ciUsername = "elysiabot"
       , ciSecret = "secretpassword" }

-- this should be much better
handleResponse :: Bool -> Response -> ByteString
handleResponse quiet (Response aid rtype params bs)
    | quiet && rtype == "Success" = empty
    | otherwise = rtype `append` ": " `append` (findWithDefault "UNKNOWN!" "Message" (fromList params))

-- handleMeetmeListEvent :: EventHandler
handleMeetmeListEvent addr cMsg ps = liftIO $ sendMsg $ B.pack $ printf "%s/#%s %s <%s> %s %s\n" confno userno callername callernum channel flags
    where
        confno = B.unpack $ findWithDefault "???" "Conference" (fromList ps)
        userno = B.unpack $ findWithDefault "?" "UserNumber" (fromList ps)
        callername = B.unpack $ findWithDefault "''" "CallerIDName" (fromList ps)
        callernum = B.unpack $ findWithDefault "???" "CallerIDNum" (fromList ps)
        channel = B.unpack $ findWithDefault "???" "Channel" (fromList ps)
        sendMsg m = sendPrivmsg addr (fromJust $ mOrigin cMsg) m
        flags :: String
        flags = intercalate " " $ map (\x -> prefixUn (map toLower x) (lookup (B.pack x) (fromList ps))) knownFlags
             where
                        prefixUn :: String -> Maybe ByteString -> String
                        prefixUn f Nothing = ""
                        prefixUn f (Just yesorno) = if yesorno == "No" then ("un" ++ f) else f
                        knownFlags :: [String]
                        knownFlags = ["Admin", "MarkedUser", "Muted", "Talking"]

main :: IO ()
main = initPlugin ["meetme"] [] onAmi

meetmeDispatch mInfo (MsgCmd cMsg server prefix cmd rest)
    | B.length rest == 0 = return ()
    | rest == "list" = withAMI_MD5 info $ do
  handleEvent "MeetmeList" (handleMeetmeListEvent addr cMsg)
  mml <- query "MeetmeList"
  liftIO $ sendMsg $ handleResponse True mml
    | "list " `B.isPrefixOf` rest = withAMI_MD5 info $ do
  handleEvent "MeetmeList" (handleMeetmeListEvent addr cMsg)
  mml <- query "MeetmeList" [("Conference", B.drop 5 rest)]
  liftIO $ sendMsg $ handleResponse True mml
    | rest == "mute" = liftIO $ sendMsg $ "usage: |meetme mute <confno> <userno>"
    | "mute " `B.isPrefixOf` rest && length scArgs == 2 = withAMI_MD5 info $ do
  mmm <- query "MeetmeMute" [("Meetme", head scArgs), ("Usernum", scArgs !! 1)]
  liftIO $ sendMsg $ handleResponse False mmm
    | "mute " `B.isPrefixOf` rest = liftIO $ sendMsg $ "usage: |meetme mute <confno> <userno>"
    | rest == "unmute" = liftIO $ sendMsg $ "usage: |meetme unmute <confno> <userno>"
    | "unmute " `B.isPrefixOf` rest && length scArgs == 2 = withAMI_MD5 info $ do
  mmu <- query "MeetmeUnmute" [("Meetme", head scArgs), ("Usernum", scArgs !! 1)]
  liftIO $ sendMsg $ handleResponse False mmu
    | "unmute " `B.isPrefixOf` rest = liftIO $ sendMsg $ "usage: |meetme unmute <confno> <userno>"
    | otherwise = liftIO $ sendMsg $ "Valid meetme subcommands: list, mute, unmute"
  where addr      = address server
        msg       = mMsg cMsg
        sendMsg m = sendPrivmsg addr (fromJust $ mOrigin cMsg) m
        scArgs = (drop 1 . B.words) rest
 
onAmi mInfo (MsgCmd cMsg server prefix cmd rest)
     | cmd == "meetme" = meetmeDispatch mInfo (MsgCmd cMsg server prefix cmd rest)
     | otherwise = return ()
onAmi _ _ = return ()

