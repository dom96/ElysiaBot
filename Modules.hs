module Modules (IrcPlugin(..), CmdFunc, loadPlugin, callCmds) where
import Network.SimpleIRC
import qualified Data.Map as M
import Data.Either
import qualified Data.ByteString.Char8 as B

import Language.Haskell.Interpreter

type CmdFunc = (IrcMessage -> IO String)

data IrcPlugin = IrcPlugin
  { pCmds :: M.Map B.ByteString CmdFunc
  , pFile :: String
  }
  
loadPlugin :: String -> IO (Either String IrcPlugin)
loadPlugin filename = do
  r <- runInterpreter $ interpretPlugin filename
  case r of
    Left err -> return $ Left $ show err
    Right cmds -> return $ Right $ IrcPlugin (M.mapKeys (\k -> B.pack k) cmds) filename

callCmd :: IrcMessage -> IrcPlugin -> IO [String]
callCmd m pl = do
  let f = M.filterWithKey (\k _ -> k == cmd) (pCmds pl) 
  mapM (\c -> (snd c) m) (M.toList f)
  where cmd = (B.words $ mMsg m) !! 0

callCmds :: IrcMessage -> [IrcPlugin] -> IO [[String]]
callCmds m pls = do
  mapM (\pl -> callCmd m pl) pls

say :: String -> Interpreter ()
say = liftIO . putStrLn

interpretPlugin :: String -> Interpreter (M.Map String CmdFunc)
interpretPlugin filename = do
  say "Loading plugin"
  loadModules [filename]
  
  setTopLevelModules [(takeWhile (/= '.') filename)]
  setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]

  a <- interpret "moduleCmds" (as :: (M.Map String CmdFunc))
  return a
