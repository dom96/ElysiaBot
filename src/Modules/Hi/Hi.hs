{-# LANGUAGE OverloadedStrings #-}
module Modules.Hi.Hi (moduleCmds, moduleRaws, onLoad) where
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import Data.Map
import Data.Maybe

moduleCmds = fromList
  [(B.pack "hi", msg)]

moduleRaws = empty

onLoad :: IO ()
onLoad = return ()

test :: Bool -> Maybe B.ByteString
test False = Nothing
test True = Just $ "True"

msg :: IrcMessage -> IO B.ByteString
msg m = do
  return $ "hello! -- from module :D " `B.append` fromJust (test True)
  
