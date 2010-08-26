{-# LANGUAGE OverloadedStrings #-}
module Modules (IrcModule(..), CmdFunc, loadMods, callCmds, toString, muteModule, unmuteModule) where
import Network.SimpleIRC

import Data.List (intercalate, foldl', delete)
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
  , mName :: B.ByteString
  , mMutedChans :: [B.ByteString]
  }

loadMods :: String -> [IrcModule]
loadMods dir =
  [ IrcModule Hi.moduleCmds Hi.moduleRaws "Hi" []
  , IrcModule Eval2.moduleCmds Eval2.moduleRaws "Eval2" []
  ]

callCmd :: Maybe B.ByteString -> IrcMessage -> IrcModule -> IrcServer -> IO [B.ByteString]
callCmd prefix m pl s
  | (getChan s m) `elem` (mMutedChans pl) = 
    -- If this module is muted in this channel, don't output anything.
    return [(B.pack "")]
  | otherwise = do
    case prefix of
      (Just p) -> do
        -- Drop the prefix, and take just the first word.
        let cmd = B.drop (B.length p) ((B.words $ mMsg m) !! 0)
        -- Filter for the correct function.
        let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mCmds pl) 
        mapM (\c -> (snd c) m) (M.toList f)
      Nothing  -> do
        let cmd = mMsg m
        let f = M.filterWithKey (\k _ -> k `B.isPrefixOf` cmd) (mRaws pl) 
        mapM (\c -> (snd c) m) (M.toList f)


callCmds :: Maybe B.ByteString -> IrcMessage -> [IrcModule] -> IrcServer -> IO [[B.ByteString]]
callCmds prefix m pls s = do
  mapM (\pl -> callCmd prefix m pl s) pls

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
  
  
