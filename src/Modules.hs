{-# LANGUAGE OverloadedStrings #-}
module Modules (IrcModule(..), CmdFunc, MessageArgs(..), loadMods, callCmds, toString, muteModule, unmuteModule) where
import Network.SimpleIRC

import Data.List (intercalate, foldl', delete)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar (MVar)

import Types

-- Modules
import qualified Modules.Hi.Hi as Hi
import qualified Modules.Eval2.Eval2 as Eval2
import qualified Modules.Hoogle.Hoogle as Hoogle
import qualified Modules.GDict.GDict as GDict
import qualified Modules.Github.Github as Github



loadMods :: String -> String -> MVar [MIrc] -> IO [IrcModule]
loadMods dir appData serversM = do
  Hi.onLoad serversM appData
  Eval2.onLoad serversM appData
  Hoogle.onLoad serversM appData
  GDict.onLoad serversM appData
  githubCmds <- Github.onLoad serversM appData

  return $
    [ IrcModule Hi.moduleCmds Hi.moduleRaws "Hi" []
    , IrcModule Eval2.moduleCmds Eval2.moduleRaws "Eval2" []
    , IrcModule Hoogle.moduleCmds Hoogle.moduleRaws "Hoogle" []
    , IrcModule GDict.moduleCmds GDict.moduleRaws "Dictionary" []
    , IrcModule githubCmds Github.moduleRaws "GitHub" []
    ]

-- TODO: Make this cleaner?
callCmd :: Maybe B.ByteString -> MVar MessageArgs ->
           IrcMessage -> IrcModule -> MIrc -> IO [B.ByteString]
callCmd prefix args m pl s = do
  dest <- getDest s m
  if dest `elem` (mMutedChans pl)
    then -- If this module is muted in this channel, don't output anything.
         return [B.empty]
    else case prefix of
           (Just p) -> do
              -- Drop the prefix
              let cmd = B.drop (B.length p) (mMsg m)
              -- Filter for the correct function.
              let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mCmds pl) 
              mapM (\c -> (snd c) args m) (M.toList f)
           Nothing  -> do
              let cmd = mMsg m
              let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mRaws pl) 
              mapM (\c -> (snd c) args m) (M.toList f)


callCmds :: Maybe B.ByteString -> MVar MessageArgs -> 
            IrcMessage -> [IrcModule] -> MIrc -> IO [[B.ByteString]]
callCmds prefix args m pls s = do
  mapM (\pl -> callCmd prefix args m pl s) pls

toString :: [IrcModule] -> String
toString mods = 
  let names = map (B.unpack . mName) mods
  in intercalate (B.unpack ", ") names

modElem :: B.ByteString -> [IrcModule] -> Bool
modElem name modules =
  foldl' (||) False (map (\m -> mName m == name) modules)

muteModule :: [IrcModule] -> B.ByteString -> B.ByteString -> Maybe [IrcModule]
muteModule modules mod chan
  | mod `modElem` modules =
    Just $ map (\m -> if mName m == mod 
                         then m {mMutedChans = chan:(mMutedChans m)}
                         else m
                ) modules
  | otherwise = Nothing
  
unmuteModule :: [IrcModule] -> B.ByteString -> B.ByteString -> Maybe [IrcModule]
unmuteModule modules mod chan
  | mod `modElem` modules =
    Just $ map (\m -> if mName m == mod 
                         then m {mMutedChans = delete chan (mMutedChans m)}
                         else m
                ) modules
  | otherwise = Nothing
  
  
