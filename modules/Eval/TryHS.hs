module TryHS (evalHS, HSResp(..)) where
import Network.Browser
import Network.HTTP
import Network.URI
import Data.Maybe
import Data.Either
import Text.JSON
import Text.JSON.String

data HSResp = HSResp { hsResult :: String
                     , hsType   :: String
                     , hsExpr   :: String
                     }

tryHaskellBaseUrl = "http://tryhaskell.org/haskell.json?method=eval&expr="

evalHS code = do
  putStrLn code
  let tryHaskellUrl = fromJust $ parseURI $ tryHaskellBaseUrl ++ code
  (uri, rsp) <- browse $ do
                setOutHandler (\a -> return ())
                setErrHandler (\a -> return ())
                request $ defaultGETRequest tryHaskellUrl
  return $ parseRSP $ rspBody rsp
  
parseRSP xs = 
  case runGetJSON readJSObject xs of
    (Right (JSObject result)) -> parseArr arr
                                 where arr = fromJSObject result
                                 
parseArr [("error", JSString xs)] = Left $ fromJSString xs
parseArr [("result", JSString res), ("type", JSString typ), ("expr", JSString ex)] =
  Right $ HSResp (fromJSString res) (fromJSString typ) (fromJSString ex)
