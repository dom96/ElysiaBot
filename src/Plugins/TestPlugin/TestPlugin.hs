import PluginUtils
main = do
  putStrLn $ "{ \"type\":\"" ++ test ++ "\" }"
  loop

loop = do
  line <- getLine
  putStrLn $ "Got line " ++ line
  loop
