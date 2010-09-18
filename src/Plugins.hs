module Plugins where
import System.IO
import System.Process
import System.FilePath
import System.Directory

import Control.Concurrent
import Control.Concurrent.MVar (MVar)
import Control.Monad

import Data.List (isPrefixOf)

import Network.SimpleIRC

data Plugin = Plugin
  { pName        :: String
  , pDescription :: String
  , pDepends     :: [String]
  , pStdout      :: Handle
  , pStderr      :: Handle
  , pStdin       :: Handle
  , pPid         :: ProcessHandle
  , pCmds        :: [String]
  }

isCorrectDir dir f = do
  r <- doesDirectoryExist (dir </> f)
  return $ r && f /= "." && f /= ".." && not ("." `isPrefixOf` f)

runPlugins :: [String] -> IO [MVar Plugin]
runPlugins ignoreModules = do
  contents  <-  getDirectoryContents "Plugins/"
  fContents <- filterM (isCorrectDir "Plugins/") contents
  
  mapM (runPlugin) fContents
  
runPlugin :: String -> IO (MVar Plugin)
runPlugin plDir = do
  currWorkDir <- getCurrentDirectory
  let plWorkDir = currWorkDir </> "Plugins/" </> plDir
      cmdExec   = "cd " ++ plWorkDir ++ " && ./run.sh"
  putStrLn $ "Running, " ++ cmdExec ++ " | Working dir = " ++ plWorkDir
  (inpH, outH, errH, pid) <- runInteractiveCommand cmdExec

  -- TODO: read the plugin.ini file.
  newMVar $ 
    Plugin plDir "" [] outH errH inpH pid []


