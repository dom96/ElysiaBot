module Types (MessageArgs(..), CmdFunc, CmdMap, IrcModule(..), Users, User(..), Plugin(..)) where
import Control.Concurrent.MVar (MVar)
import Network.SimpleIRC
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar
import System.Process
import System.IO
-- Commands.hs
data MessageArgs = MessageArgs
  { users   :: Users
  , argServers :: MVar [MIrc]
  , plugins :: [MVar Plugin]
  }

-- IrcModule
type CmdFunc = (MVar MessageArgs -> IrcMessage -> IO B.ByteString)
type CmdMap  = M.Map B.ByteString CmdFunc

data IrcModule = IrcModule
  { mCmds :: CmdMap
  , mRaws :: CmdMap
  , mName :: B.ByteString
  , mMutedChans :: [B.ByteString]
  }

-- Users
type Users = M.Map String User -- nick -> User

data User = User
  { uPass     :: String
  , uAdmin    :: Bool
  , uLoggedin :: Bool
  } deriving Show
  
-- Plugins
data Plugin = Plugin
  { pName        :: B.ByteString
  , pDescription :: B.ByteString
  , pDepends     :: [B.ByteString]
  , pLanguage    :: B.ByteString
  , pStdout      :: Handle
  , pStderr      :: Handle
  , pStdin       :: Handle
  , pPHandle     :: ProcessHandle
  , pPid         :: Maybe Int
  , pCmds        :: [B.ByteString] -- List of Elysia command bindings. e.g. test will bind to <prefix here>test
  , pCodes       :: [B.ByteString] -- List of IRC Command bindings. e.g. PRIVMSG, NOTICE...
  , pAllCodes    :: Bool     -- States whether all IRC Commands should be binded.
  , pErrors      :: [B.ByteString] -- List of all the Messages that were received through stderr.
  }
