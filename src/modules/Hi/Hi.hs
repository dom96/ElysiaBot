{-# LANGUAGE OverloadedStrings #-}
module Hi (moduleCmds, moduleRaws, msg) where
import Network.SimpleIRC.Types
import qualified Data.ByteString.Char8 as B
import Data.Map
import Data.Maybe

moduleCmds = fromList
  [(B.pack "hi", msg)]

moduleRaws = fromList
  [(B.pack "hello", sayHello)]

test :: Bool -> Maybe B.ByteString
test False = Nothing
test True = Just $ "True"

msg :: IrcMessage -> IO B.ByteString
msg m = do
  return $ "hello! -- from module :D " `B.append` fromJust (test True)
  
sayHello :: IrcMessage -> IO B.ByteString
sayHello m = do
  return $ nick `B.append` ": hey!"
  where nick = fromJust $ mNick m 
