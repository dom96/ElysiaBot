{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Control.Concurrent.MVar

import Modules

plugins = ["Hi.hs"]

onMessage :: MVar [IrcPlugin] -> EventFunc
onMessage plsMVar s m
  | msg == "|hai" = do
    sendMsg s chan "hai thar!"
  | B.isPrefixOf "|say" msg = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | msg == "|clear" = do
    putMVar plsMVar []
    
  | otherwise = do
    pls <- takeMVar plsMVar
    putMVar plsMVar pls
    ret <- callCmds m pls
    putStrLn $ show $ length $ concat ret
    mapM (\plM -> sendMsg s chan (B.pack plM)) (concat ret)
    
    return ()
    
  where chan = fromJust $ mChan m
        msg = mMsg m
        
        
freenode = IrcConfig 
  "irc.freenode.net" -- Address
  6667 -- Port
  "SimpleIRCBot" -- Nickname
  "simpleirc"  -- Username
  "simple irc" -- Realname
  ["#()"] -- Channels to join on connect

main = do
  plsMVar <- newEmptyMVar
  ret <- loadPlugin (plugins !! 0)
  either putStrLn (\r -> putMVar plsMVar [r]) ret
  let events = [(Privmsg (onMessage plsMVar))]


  connect (freenode events) False
