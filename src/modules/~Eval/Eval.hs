module Eval (moduleCmds, moduleRaws) where
import TryHS
import Network.SimpleIRC.Types
import Data.Map
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B

moduleCmds = empty

moduleRaws = fromList
  [(B.pack ">", evalCode), (B.pack ":t", getType)]
  
  
evalCode m = do
  evalResult <- evalHS (B.unpack (B.drop 1 msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\res -> return $ "=> " `B.append` (B.pack $ hsResult res))
         evalResult
  where msg = mMsg m
  
getType m  = do
  evalResult <- evalHS (B.unpack (B.drop 2 msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\res -> return $ (B.pack $ hsExpr res) `B.append` " :: " `B.append` (B.pack $ hsType res))
         evalResult
  where msg = mMsg m
