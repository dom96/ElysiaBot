{-# LANGUAGE OverloadedStrings #-}
module Modules.Github.Github (moduleCmds, moduleRaws, onLoad, ) where
import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Network
import Control.Concurrent
import Control.Concurrent.MVar
import Modules.Github.GhServer
import Types

moduleCmds = M.empty

moduleRaws = M.empty

addCommitUser :: MVar MessageArgs -> IrcMessage -> IO B.ByteString
addCommitUser argsMVar m =
  return $ mMsg m

onLoad serversM appDatDir = do 
  s <- listenOn (PortNumber 3456)
  forkIO (listenLoop s serversM)
  putStrLn "Listening on 3456 for github connections."
  
  return $ M.fromList [(B.pack "commits add", addCommitUser)]
