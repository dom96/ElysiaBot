{-# LANGUAGE OverloadedStrings #-}
module Modules.Hi.Hi (moduleCmds, moduleRaws, onLoad) where
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar (MVar)
import Data.Map
import Data.Maybe
import Types

moduleCmds = fromList
  [(B.pack "hi", msg)]

moduleRaws = empty

onLoad :: MVar [MIrc] -> String -> IO ()
onLoad _ _ = return ()

test :: Bool -> Maybe B.ByteString
test False = Nothing
test True = Just $ "True"

msg :: MVar MessageArgs -> IrcMessage -> IO B.ByteString
msg _ m = do
  return $ "hello! -- from module :D " `B.append` fromJust (test True)
  
