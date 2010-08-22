{-# LANGUAGE PackageImports #-}
module Users (readUsers, saveUsers, setLoggedin, getLoggedin, Users, User(..)) where
import Data.ConfigFile
import System.IO  (writeFile)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List  (intercalate)
import "mtl" Control.Monad.Error
type Users = M.Map String User -- nick -> (pass, admin?)

data User = User
  { uPass     :: String
  , uAdmin    :: Bool
  , uLoggedin :: Bool
  } deriving Show

readUsers filename = do
  readfile emptyCP filename >>= getData . handleError

handleError :: Either (CPErrorData, [Char]) a -> a
handleError x =
  case x of
    Left (ParseError err, err') -> error $ "Parse error: \n"
                                   ++ err ++ "\n" ++ err'
    Left err                    -> error $ show err
    Right c                     -> c

getData :: ConfigParser -> IO Users
getData cp = do
  users <- mapM (getSections cp) (sections cp)
  
  return $! M.fromList $ users

getSections :: ConfigParser -> String -> IO (String, User)
getSections cp section = do
  user <- runErrorT $ do
    pass  <- get cp section "pass"
    admin  <- get cp section "admin"
  
    return $! (section, (User pass admin False))
  return $ handleError user

setLoggedin :: Users -> String -> String -> Bool -> Either String Users
setLoggedin u nick pass state
  | M.member nick u =
    let usr = fromJust $ M.lookup nick u
    in if (uPass usr) == pass
         then Right $ M.adjust (\a -> a {uLoggedin = state}) nick u
         else Left $ "Wrong password"
  | otherwise = Left $ "User doesn't exist"
  
renderUser :: String -> User -> String
renderUser nick u =
  section ++ p ++ a ++ "\n"
  where section = "[" ++ nick ++ "]\n"
        p       = "pass  = " ++ (uPass u) ++ "\n"
        a       = "admin = " ++ (show $ uAdmin u) ++ "\n"
  
renderUsers :: Users -> String  
renderUsers u = 
  M.foldWithKey (\k a b -> b ++ renderUser k a) "" u
  
saveUsers :: Users -> String -> IO ()
saveUsers = writeFile . renderUsers


-- Info generation
getLoggedin :: Users -> String
getLoggedin u =
  users
  where online = M.filter (\a -> uLoggedin a) (u)
        users  = intercalate ", " (M.keys online) 


