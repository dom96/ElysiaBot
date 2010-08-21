module Eval2 (moduleCmds, moduleRaws) where
import Network.SimpleIRC.Types
import Data.Map
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Concurrent
import System.IO

moduleCmds = empty

moduleRaws = fromList
  [(B.pack "> ", evalCode), (B.pack ":t ", getType)]



evalCode m = do
  evalResult <- evalM (B.unpack (B.drop 2 msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(_, _, res) -> return $ "=> " `B.append` (B.pack $ res))
         evalResult
  where msg = mMsg m

getType m  = do
  evalResult <- evalM ("showsTypeRep (typeOf (" ++ code ++ ")) \"\"")
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(expr, _, res) -> 
            return $ (B.pack $ code) `B.append` " :: "
                     `B.append` (B.pack $ removeQuotes res))
         evalResult
  where msg = mMsg m
        code = removeStart ' ' (B.unpack (B.drop 2 msg))
       
formatErr err
  | length e > 1 = 
    let second = (take 28 $ e !! 1) ++ "..."
    in first ++ "\n" ++ second
  | otherwise = first
  where e = take 2 $ lines err
        first  = (take 350 $ e !! 0) 

parseRet ret code 
  | (lines ret !! 0) /= code  = return $ Left $ formatErr ret
  | (length $ lines ret) == 2 = return $ Left "Time limit exceeded"
  | (length $ lines ret) > 2  = do 
    let (expr, typ, result) = (lines ret !! 0, lines ret !! 1, lines ret !! 2)
    return $ Right (expr, typ, result)
  | otherwise                 = return $ Left $ "Unknown error. Received " ++ ret

evalM code = do
  (inp,out,err,pid) <- runInteractiveCommand $ "mueval -i --expression '" ++ code ++ "'"
  ret <- hGetContents out
  parseRet ret code
  
removeStart p (x:xs)
  | x == p    = xs
  | otherwise = x:xs

removeQuoteStart = removeStart '\"'

removeQuoteEnd   xs =
  if drop (length xs - 1) xs == "\""
    then take (length xs - 1) xs
    else xs

removeQuotes = removeQuoteEnd . removeQuoteStart    





