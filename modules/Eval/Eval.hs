module Eval (moduleCmds, moduleRaws) where
import TryHS
import Network.SimpleIRC.Types
import Data.Map
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Char8 as B

moduleCmds = empty

moduleRaws = fromList
  [(B.pack ">", evalCode)]
  
  
evalCode m = do
  evalResult <- evalHS (B.unpack (B.dropWhile (/= ' ') msg))
  either (\err -> return $ "Error: " `B.append` (B.pack err))
         (\res -> return $ "=> " `B.append` (B.pack $ hsResult res))
         evalResult
  where msg = mMsg m
