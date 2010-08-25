module Modules (IrcModule(..), CmdFunc, loadMods, callCmds, toString) where
import Network.SimpleIRC

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

-- Modules
import qualified Modules.Hi.Hi as Hi
import qualified Modules.Eval2.Eval2 as Eval2

type CmdFunc = (IrcMessage -> IO B.ByteString)
type CmdMap  = M.Map B.ByteString CmdFunc

data IrcModule = IrcModule
  { mCmds :: CmdMap
  , mRaws :: CmdMap
  , mName :: String
  }

loadMods :: String -> [IrcModule]
loadMods dir =
  [ IrcModule Hi.moduleCmds Hi.moduleRaws "Hi"
  , IrcModule Eval2.moduleCmds Eval2.moduleRaws "Eval2"
  ]

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
