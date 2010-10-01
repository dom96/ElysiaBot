import PluginUtils
main = do
  sendPID
  pluginLoop recvMsg

recvMsg :: Message -> IO ()
recvMsg rcvMsg = do
  putStrLn $ "TestPlugin: " ++ show rcvMsg

