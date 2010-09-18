
main = do
  putStrLn "{ \"type\":\"success\" }"
  loop

loop = do
  line <- getLine
  putStrLn $ "Got line " ++ line
  loop
