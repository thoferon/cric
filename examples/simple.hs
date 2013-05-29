import Cric.Platform
import System.IO
import Control.Monad.Trans

config :: Cric ()
config = do
  chmod 0o777 "/tmp"
  createUser "dummy" $ duo { groups = ["dummies", "idiots"] }
  withEnv [("blah", "1")] . inDir "/tmp" . asUser "root" $ do
    installPackage "ruby"
    run "whoami"
  sendFile "examples/file" "/tmp/blah" $ dfto { md5Hash = Just "6a8f5078116f848165228ae0e466ac6d" }
  result <- exec "ls -la"
  liftIO $ print result

main :: IO ()
main = do
  putStr "Hostname: "
  hFlush stdout
  h <- getLine

  putStr "User: "
  hFlush stdout
  u <- getLine

  putStr "Password: "
  hFlush stdout
  p <- getLine

  auto <- autoServer
  let server = auto { hostname = h, user = u, password = p, authType = PasswordAuthentication }

  print server
  putStrLn "---------------------"

  install config debugLogger defaultContext server
  -- or you can try
  -- config `installOn` server

  putStrLn "That's all folks !"
