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
  [(B.pack ">", evalCode), (B.pack ":t", getType)]
  
parseRet ret code 
  | (lines ret !! 0) /= code = return $ Left ret
  | (length $ lines ret) == 2 = return $ Left "Time limit exceeded"
  | otherwise = do let (expr, typ, result) = (lines ret !! 0, lines ret !! 1, lines ret !! 2)
                   return $ Right (expr, typ, result)

evalM code = do
  (inp,out,err,pid) <- runInteractiveCommand $ "mueval -i --expression '" ++ code ++ "'"
  ret <- hGetContents out
  parseRet ret code

evalCode m = do
  evalResult <- evalM (B.unpack (B.drop 1 msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(_, _, res) -> return $ "=> " `B.append` (B.pack $ res))
         evalResult
  where msg = mMsg m
  
getType m  = do
  evalResult <- evalM ("showsTypeRep (typeOf (" ++ code ++ ")) \"\"")
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\(expr, _, res) -> 
            return $ (B.pack $ code) `B.append` " :: " `B.append` (B.pack $ res))
         evalResult
  where msg = mMsg m
        code = (B.unpack (B.drop 2 msg))
