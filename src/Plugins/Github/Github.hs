{-# LANGUAGE OverloadedStrings #-}
import PluginUtils
import Network.SimpleIRC.Messages

import qualified Data.ByteString.Char8 as B

import Control.Concurrent
import Network

import GhServer

main = do
  s <- listenOn (PortNumber 3456)
  putStrLn "Listening on 3456 for github connections."
  forkIO (listenLoop s)
  initPlugin [] [] (\_ _ -> return ())
