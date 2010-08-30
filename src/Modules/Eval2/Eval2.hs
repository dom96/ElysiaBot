{-# LANGUAGE OverloadedStrings #-}
module Modules.Eval2.Eval2 (moduleCmds, moduleRaws) where
import Network.SimpleIRC.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent
import System.IO

moduleCmds = M.empty

moduleRaws = M.fromList
  [(B.pack "> ", evalCode)
--  , (B.pack ":t ", getType)
  ]

evalCode m = do
  evalResult <- evalM (B.unpack (B.drop 2 msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(_, _, res) -> return $ "=> " `B.append` (B.pack $ limitMsg 200 res))
         evalResult
  where msg = mMsg m

getType m  = do
  evalResult <- evalM ("showsTypeRep (typeOf (" ++ code ++ ")) \"\"")
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(expr, _, res) -> 
            return $ (B.pack $ code) `B.append` " :: "
                     `B.append` (B.pack $ removeQuotes $ limitMsg 200 res))
         evalResult
  where msg = mMsg m
        code = removeStart ' ' (B.unpack (B.drop 2 msg))
 
formatErr err
  | length e > 1 = 
    let second = (limitMsg 28 $ e !! 1)
    in first ++ "\n" ++ second
  | otherwise = first
  where e = take 2 $ lines err
        first  = (limitMsg 350 $ e !! 0) 

limitMsg limit xs = 
  if length xs > limit
    then take limit xs ++ "..."
    else xs

parseRet ret code 
  | (lines ret !! 0) /= code  = return $ Left $ formatErr ret
  | (length $ lines ret) > 2  = do 
    let (expr, typ, result) = (lines ret !! 0, lines ret !! 1, lines ret !! 2)
    return $ Right (expr, typ, result)
  | otherwise                 = return $ Left $ "Unknown error. Received " ++ ret

escapeCode code = 
  concatMap (\c -> case c of '\"' -> "\\\""
                             '`' -> "\\`"
                             otherwise -> [c]) code

evalM code = do
  putStrLn $ "Executing...\n  mueval -i --expression \"" ++ (escapeCode code) ++ "\""
  (inpH, outH, errH, pid) <- runInteractiveProcess "mueval" ["-i","--expression", code] Nothing Nothing
  waitForProcess pid
  ret <- hGetContents outH
  err <- hGetContents errH
  putStrLn $ "Got contents...\n  " ++ ret
  putStrLn $ "Got error...\n " ++ err
  
  if null err
    then do parsed <- parseRet ret code
            return parsed
    else return $ Left $ limitMsg 200 err
  
removeStart p (x:xs)
  | x == p    = xs
  | otherwise = x:xs

removeQuoteStart = removeStart '\"'

removeQuoteEnd   xs =
  if drop (length xs - 1) xs == "\""
    then take (length xs - 1) xs
    else xs

removeQuotes = removeQuoteEnd . removeQuoteStart    





