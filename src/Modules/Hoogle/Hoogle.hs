{-# LANGUAGE OverloadedStrings #-}
module Modules.Hoogle.Hoogle (moduleCmds, moduleRaws, escape) where
import Network.SimpleIRC
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent
import System.IO

moduleCmds = M.fromList
  [(B.pack "hoogle", find)]

moduleRaws = M.fromList
  [(B.pack ":t ", getType)]

find m = do
  evalResult <- findM searchTerm
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\res -> return $ (B.pack $ limitLines 200 4 res))
         evalResult
  where msg = mMsg m
        searchTerm = (B.unpack $ B.unwords (drop 1 $ B.words msg))

getType m  = do
  evalResult <- findM (code)
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\res -> 
            return $ (B.pack $ limitLines 200 1 res))
         evalResult
  where msg = mMsg m
        code = removeStart ' ' (B.unpack (B.drop 2 msg))

limitLines lenLimit lineLimit xs =
   unlines $ map (limitMsg lenLimit) $ take lineLimit (lines xs)

limitMsg limit xs = 
  if length xs > limit
    then take limit xs ++ "..."
    else xs

escape :: String -> String -> String
escape "-" ('>':rest) = '#':(escape ">" rest)

escape _ (cur:rest)   = cur:(escape [cur] rest)
escape _ xs           = xs

findM code = do
  putStrLn $ "Executing...\n  hoogle " ++ (escape [] code)
  (inpH, outH, errH, pid) <- runInteractiveProcess "hoogle" (words $ escape [] code) Nothing Nothing
  waitForProcess pid
  ret <- hGetContents outH
  err <- hGetContents errH
  putStrLn $ "Got contents...\n  " ++ ret
  putStrLn $ "Got error...\n " ++ err
  
  if null err
    then return $ Right ret
    else return $ Left $ limitMsg 200 err
  
removeStart p (x:xs)
  | x == p    = xs
  | otherwise = x:xs





