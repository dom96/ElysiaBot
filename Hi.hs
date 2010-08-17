module Hi (moduleCmds, msg) where
import Network.SimpleIRC.Types
import Data.Map

moduleCmds = fromList
  [("hu", msg)]
  
  
msg :: IrcMessage -> IO String
msg m = do
  return "hello! -- from module :D"
