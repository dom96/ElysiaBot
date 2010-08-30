module Modules.GDict.GDictParse (lookupDict, ParsedDict(..)) where
import Network.Browser
import Network.HTTP
import Network.URI
import Data.Maybe
import Data.Either
import Text.JSON
import Text.JSON.String
import Text.Regex
import Data.List

data ParsedDict = ParsedDict
  { related :: [String]
  , meanings :: [String]
  , unknown :: [(String, JSValue)]
  } deriving Show

googleDictBaseURL = "http://www.google.com/dictionary/json?sl=en&tl=en&callback=c&q="

lookupDict word = do
  let googleDictUrl = fromJust $ parseURI $ 
                      escapeURIString isUnescapedInURI $ googleDictBaseURL ++ word
  (uri, rsp) <- browse $ do
                setOutHandler (\a -> return ())
                setErrHandler (\a -> return ())
                request $ defaultGETRequest googleDictUrl
  let taken = takeCallback $ rspBody rsp
  let re = mkRegex "\\\\(x([0-9A-Fa-f]{2}))"
  if isJust taken
    then return $ parseRSP $ subRegex re (fromJust taken) "\\u00\\2" 
    else return $ Left "Google dictionaries API was changed, according to my high tech ChangeDetect technology. Callback thingie could not be found :(."

parseRSP :: String -> Either String ParsedDict
parseRSP xs = 
  case runGetJSON readJSObject xs of
    (Right (JSObject result)) -> 
      if not $ null parsedArr
       then Right $ parseEntries (ParsedDict [] [] []) parsedArr
       else Left "No results found."
      where parsedArr = parseArr arr
            arr = fromJSObject result
    (Left err)                -> Left err

parseArr :: [(String, JSValue)] -> [JSValue]
parseArr arr
  | length arr > 3 =
    case (arr !! 3) of
      ("primaries", primaries) -> 
        parseArr $ fromJSObject $
          frmJSObject ((fromJSArray primaries) !! 0)
      
  | length arr > 2 = 
    case (arr !! 2) of
      ("entries",   entries  ) -> fromJSArray entries
      ("targetLanguage", _   ) -> []
  | otherwise      = []

fromJSArray (JSArray arr) = arr
frmJSObject (JSObject obj) = obj
frmJSString (JSString str) = fromJSString str

getText :: (String, JSValue) -> String
getText (_, arr) = 
  let f = filter (\t -> (fst t) == "text") list
  in frmJSString $ snd $ f !! 0

  where l    = fromJSArray $ arr
        list = fromJSObject $ frmJSObject $ (l !! 0)


parseEntry :: ParsedDict -> [(String, JSValue)] -> ParsedDict
parseEntry dict (typ:rest)
  | (fst typ) == "type" && (frmJSString $ snd typ) == "related" =
    dict { related = getText (rest !! 0):related dict }
  | (fst typ) == "type" && (frmJSString $ snd typ) == "meaning" =
    dict { meanings = getText (rest !! 0):meanings dict }
  | otherwise = dict { unknown = (rest !! 0):unknown dict }

parseEntries :: ParsedDict -> [JSValue] -> ParsedDict
parseEntries dict values =
  dNew { related = reverse $ related dNew, meanings = reverse $ meanings dNew }
  where dNew = foldl' 
               (\a b -> parseEntry a (fromJSObject $ frmJSObject b))
               dict values

takeFCallback :: String -> Maybe String
takeFCallback ('c':'(':xs) = Just xs
takeFCallback xs           = Nothing

takeLCallback :: String -> Maybe String
takeLCallback xs
  | drop (length xs - 10) xs == ",200,null)" =
    Just $ take (length xs - 10) xs
  | otherwise                                = Nothing
  
takeCallback xs           = 
  if isJust $ fC
    then takeLCallback $ fromJust fC
    else Nothing
  where fC = takeFCallback xs
