{-# LANGUAGE OverloadedStrings #-}

import Network (PortID(..))
import Network.Socket (HostName(..))
import Network.Protocol.XMPP (runClient, bindJID, getStanza, putStanza, parseJID, getSession, runXMPP, emptyIQ, strNode, messageType, JID(..), Server(..), XMPP(..), ReceivedStanza(..), MessageType(..), Message(..), Error(..), IQ(..), IQType(..), Session(..))
import qualified Data.Text as T
import Data.Maybe (fromJust, catMaybes)
import Control.Arrow ((***))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>))
import Data.Monoid (mappend, Monoid(..))
import Data.XML.Types (Element(..), Node(..), Content(..), Name(..))

import qualified Data.ByteString.Char8 as B
import PluginUtils hiding (Message)

loop = do
        _ <- runPersistentClient (botJID, password) (xmpphost, portnum) $ forever $ do
                message <- getStanza
                case message of
                        ReceivedMessage msg | messageType msg /= MessageError && messageFrom msg == parseJID (T.pack "secretsender@example.org/jabberresource") ->
                          let (dst, text) = (dstAndMsg . T.unpack . extractMsg) msg in
                          liftIO $ sendPrivmsg irchost dst text
                        _ -> return ()
        return ()
        where   -- these should all be in a config file
                botJID = fromJust $ parseJID (T.pack "secretreceiver@example.org")
                password = T.pack "secretpassword"
                xmpphost = "jabberserver.example.net"
                portnum = PortNumber 5222
                irchost = B.pack "localhost"
                dstAndMsg = (B.pack *** B.pack) . (unwords *** unwords) . splitAt 1 . words

main = do
  forkIO $ loop
  initPlugin [] [] (\_ _ -> return ())

-- this is lifted from EasyXMPP
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

extractBody :: [Element] -> [Node]
extractBody [Element (Name ("body") _ _) _ ns] = ns
extractBody _ = []

extractContent :: Node -> Maybe T.Text
extractContent (NodeContent (ContentText txt)) = Just txt
extractContent _ = Nothing

extractMsg :: Message -> T.Text
extractMsg = T.concat . catMaybes . map extractContent . extractBody . messagePayloads

sendPings :: Integer -> Session -> IO ()
sendPings seconds s = forever send where
  send = do
    -- Ignore errors
    _ <- runXMPP s $ putStanza ping
    threadDelay $ fromInteger $ 1000000 * seconds
  ping = (emptyIQ IQGet)
           { iqPayload = Just (Element pingName [] []) }

pingName :: Name
pingName = Name "ping" (Just "urn:xmpp:ping") Nothing

runPersistentClient :: (JID, T.Text) -> (HostName, PortID) -> XMPP a -> IO (Either Error a)
runPersistentClient (myJID, pass) (hostname, port) act =
  runClient server myJID username pass $ do
    -- Some servers will close the XMPP connection after some period
    -- of inactivity. For this example, we'll simply send a "ping" every
    -- 60 seconds
    _ <- getSession >>= liftIO . forkIO . sendPings 60

    _ <- bindJID myJID
    act

  where
    server = Server (JID Nothing (jidDomain myJID) Nothing) hostname port
    username = case strNode <$> jidNode myJID of
      Just x -> x
      Nothing -> error $ "JID must include a username"
