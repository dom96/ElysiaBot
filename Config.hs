{-# LANGUAGE PackageImports #-}
module Config (ConfigServer(..), ConfigInfo(..), readConfig) where
import Data.ConfigFile
import Data.List
import Data.Char
import Data.Either
import "mtl" Control.Monad.Error

data ConfigServer = ConfigServer
  { cnfAddr  :: String
  , cnfPort  :: Int
  , cnfChans :: [String]
  } deriving Show

data ConfigInfo = ConfigInfo
  { cnfNick    :: String
  , cnfUser    :: String
  , cnfReal    :: String
  , cnfServers :: [ConfigServer]
  } deriving Show

readConfig filename = do
  readfile emptyCP filename >>= getData . handleError

handleError :: Either (CPErrorData, [Char]) a -> a
handleError x =
  case x of
    Left (ParseError err, err') -> error $ "Parse error: \n"
                                   ++ err ++ "\n" ++ err'
    Left err                    -> error $ show err
    Right c                     -> c

getData cp = do
  config <- runErrorT $ do
    nick <- get cp "DEFAULT" "nick"
    user <- get cp "DEFAULT" "user"
    real <- get cp "DEFAULT" "real"
    
    servers <- mapM (getSections cp) (sections cp)
    
    return $! ConfigInfo nick user real servers

  return $ handleError config
  
getSections cp section 
  | "server" `isPrefixOf` section = do
    server <- runErrorT $ do
      addr  <- get cp section "address"
      port  <- get cp section "port"
      chans <- get cp section "chans"
    
      return $! ConfigServer (addr :: String) (readInt "port" port) 
                             (readLst "chans" chans)
    return $ handleError server
      
  | otherwise                     =
    error $ "Invalid section, expected \"server\", got \"" ++ section ++ "\"."
    
readInt :: (Num a, Read a) => String -> String -> a
readInt _ x | all isDigit x = read x
readInt opt _ = error $ opt ++ " must be a number."

readLst :: String -> String -> [String]
readLst opt [] = []
readLst opt xs
  | "," `isPrefixOf` xs = error $ "Invalid list for \"" ++ opt ++ "\""
  | otherwise = 
    (noSpace first) : (readLst opt $ drop 1 $ dropWhile (/= ',') second)
    where (first, second) = break (== ',') xs
          noSpace f       = takeWhile (/= ' ') (dropWhile (== ' ') f)

forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x
