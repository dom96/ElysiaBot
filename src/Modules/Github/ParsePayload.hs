module Modules.Github.ParsePayload (Author(..), Repo(..), Commit(..), Payload(..), parseAll) where
import Text.JSON
import Text.JSON.String
import Text.JSON.Types
import Data.List
import Data.Maybe
import Data.Ratio ((%))

data Author = Author
  { a_name  :: String
  , email   :: String
  } deriving (Show)

data Repo = Repo
  { created_at    :: String
  , open_issues   :: Rational
  , pushed_at     :: String
  , forks         :: Rational
  , description   :: String
  , has_issues    :: Bool
  , fork          :: Bool
  , has_downloads :: Bool
  , private       :: Bool
  , owner         :: Author
  , r_url         :: String
  , name          :: String
  , watchers      :: Rational
  , homepage      :: String
  , has_wiki      :: Bool
  } deriving (Show)

data Commit = Commit
  { message    :: String
  , removed    :: [String]
  , url        :: String
  , timestamp  :: String
  , modified   :: [String]
  , added      :: [String]
  , c_id       :: String
  , author     :: Author
  } deriving (Show)

data Payload = Payload
  { repository :: Repo 
  , ref        :: String
  , commits    :: [Commit]
  , before     :: String
  , forced     :: Bool
  , p_compare  :: String
  , after      :: String
  , invalid    :: Maybe [String] -- Invalid objects.
  } deriving (Show)

defaultAuthor  = Author "" ""
defaultRepo    = Repo
  { created_at = ""
  , open_issues = 0 % 0
  , pushed_at  = ""
  , forks      = 0 % 0
  , description = ""
  , has_issues = False
  , fork       = False
  , has_downloads = False
  , private       = False
  , owner         = defaultAuthor
  , r_url         = ""
  , name          = ""
  , watchers      = 0 % 0
  , homepage      = ""
  , has_wiki      = False
  }
defaultCommit = Commit
  { message    = ""
  , removed    = []
  , url        = ""
  , timestamp  = ""
  , modified   = []
  , added      = []
  , c_id       = ""
  , author     = defaultAuthor
  }

defaultPayload = Payload 
  { repository = defaultRepo
  , ref        = ""
  , commits    = []
  , before     = ""
  , forced     = False
  , p_compare  = ""
  , after      = ""
  , invalid    = Nothing
  }

parseAll :: [Char] -> Either String Payload
parseAll body = either (\a -> Left a) parseJSON (parseBody body)


parseBody body
  | "payload=" `isPrefixOf` body =
    Right $ drop 8 body
  | otherwise                    = Left $ "Invalid body. Got: \n" ++ body

parseJSON body =
  case decode body of
    (Ok result) -> Right $ parseObjects (result :: JSValue)
    (Error err) -> Left err
    

parseObjects (JSObject (JSONObject val)) =  
  foldl' parseObject defaultPayload val
  --map (\(name, _) -> name) val

parseObject payload ("repository", (JSObject (JSONObject val))) = 
  payload { repository = foldl' parseRepo defaultRepo val}
parseObject payload ("ref", (JSString (JSONString val))) = payload {ref = val}
parseObject payload ("commits", (JSArray val)) = 
  payload { commits = reverse $ map parseCommitObjs val}
parseObject payload ("before", (JSString (JSONString val))) = 
  payload { before = val}
parseObject payload ("forced", (JSBool val)) = 
  payload { forced = val}
parseObject payload ("after", (JSString (JSONString val))) = 
  payload { after = val}  
parseObject payload ("compare", (JSString (JSONString val))) = 
  payload { p_compare = val}  
parseObject payload (name, _) = 
  if not $ isJust $ invalid payload
    then payload {invalid = Just [name]}
    else let inv = fromJust $ invalid payload
         in payload {invalid = Just $ name:inv}

parseRepo repo ("created_at", (JSString (JSONString val))) = 
  repo { created_at = val}
parseRepo repo ("open_issues", (JSRational _ val)) = 
  repo { open_issues = val}
parseRepo repo ("pushed_at", (JSString (JSONString val))) = 
  repo { pushed_at = val}
parseRepo repo ("forks", (JSRational _ val)) = 
  repo { forks = val}
parseRepo repo ("description", (JSString (JSONString val))) = 
  repo { description = val}
parseRepo repo ("has_issues", (JSBool val)) = 
  repo { has_issues = val}
parseRepo repo ("fork", (JSBool val)) = 
  repo { fork = val}
parseRepo repo ("has_downloads", (JSBool val)) = 
  repo { has_downloads = val}
parseRepo repo ("owner", JSObject (JSONObject val)) = 
  repo { owner = foldl' parseAuthor defaultAuthor val }
parseRepo repo ("private", (JSBool val)) = 
  repo { private = val}
parseRepo repo ("url", (JSString (JSONString val))) = 
  repo { r_url = val}
parseRepo repo ("name", (JSString (JSONString val))) = 
  repo { name = val}
parseRepo repo ("watchers", (JSRational _ val)) = 
  repo { watchers = val}
parseRepo repo ("homepage", (JSString (JSONString val))) = 
  repo { homepage = val}
parseRepo repo ("has_wiki", (JSBool val)) = 
  repo { has_wiki = val}

parseCommitObjs (JSObject (JSONObject val)) =
  foldl' parseCommit defaultCommit val

parseCommit commit ("message", (JSString (JSONString val))) =
  commit { message = val}
parseCommit commit ("removed", (JSArray val)) =
  commit { removed = map fromJSStr val}
parseCommit commit ("url", (JSString (JSONString val))) =
  commit { url = val}
parseCommit commit ("timestamp", (JSString (JSONString val))) =
  commit { timestamp = val}
parseCommit commit ("modified", (JSArray val)) =
  commit { modified = map fromJSStr val}
parseCommit commit ("added", (JSArray val)) =
  commit { added = map fromJSStr val}
parseCommit commit ("id", (JSString (JSONString val))) =
  commit { c_id = val}
parseCommit commit ("author", JSObject (JSONObject val)) = 
  commit { author = foldl' parseAuthor defaultAuthor val }

parseAuthor author ("email", (JSString (JSONString val))) = 
  author { email = val}
parseAuthor author ("name", (JSString (JSONString val))) = 
  author { a_name = val}

fromJSStr :: JSValue -> String
fromJSStr (JSString (JSONString val)) = val
