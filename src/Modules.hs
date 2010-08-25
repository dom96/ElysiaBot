module Modules (IrcModule(..), CmdFunc, loadModule, loadMods, callCmds, prettyError, toString) where
import Network.SimpleIRC
import qualified Data.Map as M
import Data.Either
import qualified Data.ByteString.Char8 as B
import Language.Haskell.Interpreter
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Data.List (intercalate, isPrefixOf)

type CmdFunc = (IrcMessage -> IO B.ByteString)
type CmdMap  = M.Map B.ByteString CmdFunc

data IrcModule = IrcModule
  { mCmds :: CmdMap
  , mRaws :: CmdMap
  , mName :: String
  , mFile :: String
  }

loadModule :: String -> IO (Either InterpreterError IrcModule)
loadModule filename = do
  r <- runInterpreter $ interpretModule filename
  case r of
    Left err -> return $ Left $ err
    Right (cmds, raws) -> return $ Right $ IrcModule 
        (M.mapKeys (\k -> k) cmds) (M.mapKeys (\k -> k) raws) (takeBaseName filename) filename

moduleDir :: FilePath -> FilePath -> FilePath
moduleDir mDir dir = mDir </> dir </> (takeBaseName dir ++ ".hs")

isCorrectDir dir f = do
  r <- doesDirectoryExist (dir </> f)
  return $ r && f /= "." && f /= ".." && not ("." `isPrefixOf` f) && not ("~" `isPrefixOf` f)

loadMods :: String -> IO ([InterpreterError], [IrcModule])
loadMods dir = do
  mods  <- getDirectoryContents dir
  modsF <- filterM (isCorrectDir dir) mods
  es    <- mapM (loadModule . (moduleDir dir)) modsF
  return $ partitionEithers es


callCmd :: Maybe B.ByteString -> IrcMessage -> IrcModule -> IO [B.ByteString]
callCmd (Just prefix) m pl = do
  let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mCmds pl) 
  mapM (\c -> (snd c) m) (M.toList f)
  where cmd = B.drop (B.length prefix) ((B.words $ mMsg m) !! 0)
  
callCmd Nothing m pl       = do
  --B.putStrLn $ (B.pack "cmd = ") `B.append` cmd
  let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mRaws pl) 
  mapM (\c -> (snd c) m) (M.toList f)
  where cmd = mMsg m

callCmds :: Maybe B.ByteString -> IrcMessage -> [IrcModule] -> IO [[B.ByteString]]
callCmds prefix m pls = do
  mapM (\pl -> callCmd prefix m pl) pls

toString :: [IrcModule] -> String
toString mods = 
  let names = map (mName) mods
  in intercalate ", " names

say :: String -> Interpreter ()
say = liftIO . putStrLn

prettyError :: InterpreterError -> String
prettyError (UnknownError errM)   = "Unknown Error: " ++ errM
prettyError (WontCompile  (x:xs)) = 
  if length xs /= 0
    then "Wont Compile: " ++ (errMsg x) ++ "\n... " ++ (show $ length xs) ++ " more errors." 
    else "Wont Compile: " ++ (errMsg x)
prettyError (NotAllowed   errM)   = "Not Allowed: " ++ errM
prettyError (GhcException  errM)   = "Ghc Exception: " ++ errM

interpretModule :: String -> Interpreter (CmdMap, CmdMap)
interpretModule filename = do
  say $ "Loading module " ++ filename
  set [languageExtensions := [OverloadedStrings], searchPath := [".", "modules", takeDirectory filename]]
  loadModules [filename]
  
  exts <- get searchPath
  say $ show exts
  
  mods <- getLoadedModules
  say $ show mods
  
  setTopLevelModules [(takeBaseName filename)]
  setImportsQ [("Prelude", Nothing), ("Data.Map", Nothing),
      ("Data.ByteString.Char8", Just "B"), ("Data.ByteString.Internal", Nothing), ("Modules", Nothing)]

  cmds <- interpret "moduleCmds" (as :: CmdMap)
  raws <- interpret "moduleRaws" (as :: CmdMap)
  return (cmds, raws)
