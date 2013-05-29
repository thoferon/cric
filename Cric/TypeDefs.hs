module Cric.TypeDefs (
  Server(..)
  , AuthenticationType(..)
  , Logger
  , LogLevel(..)
  , Context(..)
  , SshSession(..)
  , defaultServer
  , defaultContext
  , stdoutLogger
  , debugLogger
  , autoServer
  ) where

import System.Directory
import System.FilePath

import Network.SSH.Client.LibSSH2 (Session, execCommands, scpSendFile)
import qualified Data.ByteString.Lazy as BL

-- For tests, see tests/SpecHelpers.hs
class SshSession s where
  sshExecCommands :: s -> [String] -> IO (Int, [BL.ByteString])
  sshSendFile :: s -> Int -> FilePath -> FilePath -> IO Integer

instance SshSession Session where
  sshExecCommands = execCommands
  sshSendFile = scpSendFile

data LogLevel = LDebug | LInfo | LNotice | LWarning | LError | LPanic
              deriving (Show, Eq)

type Logger = LogLevel -> String -> IO ()

stdoutLogger :: Logger
stdoutLogger LDebug   _   = return ()
stdoutLogger LInfo    msg = putStrLn $ "[Info] "    ++ msg
stdoutLogger LNotice  msg = putStrLn $ "[Notice] "  ++ msg
stdoutLogger LWarning msg = putStrLn $ "[Warning] " ++ msg
stdoutLogger LError   msg = putStrLn $ "[Error] "   ++ msg
stdoutLogger LPanic   msg = putStrLn $ "[Panic] "   ++ msg

debugLogger :: Logger
debugLogger LDebug   msg = putStrLn $ "[Debug] "   ++ msg
debugLogger LInfo    msg = putStrLn $ "[Info] "    ++ msg
debugLogger LNotice  msg = putStrLn $ "[Notice] "  ++ msg
debugLogger LWarning msg = putStrLn $ "[Warning] " ++ msg
debugLogger LError   msg = putStrLn $ "[Error] "   ++ msg
debugLogger LPanic   msg = putStrLn $ "[Panic] "   ++ msg

data AuthenticationType = KeysAuthentication
                        | PasswordAuthentication
                        deriving (Show, Eq)

data Context = Context {
  currentUser :: String
  , currentDir :: FilePath
  , currentEnv :: [(String, String)]
} deriving (Eq, Show)

defaultContext :: Context
defaultContext = Context {
  currentUser = ""
  , currentDir = ""
  , currentEnv = []
}

data Server = Server {
  hostname :: String
  , port :: Int
  , user :: String
  , password :: String
  , knownHostsPath :: FilePath
  , publicKeyPath :: FilePath
  , privateKeyPath :: FilePath
  , passphrase :: String
  , authType :: AuthenticationType
} deriving (Eq, Show)

defaultServer :: Server
defaultServer = Server {
  hostname = "127.0.0.1"
  , port = 22
  , user = "root"
  , password = ""
  , knownHostsPath = "known_hosts"
  , publicKeyPath = "id_rsa.pub"
  , privateKeyPath = "id_rsa"
  , passphrase = ""
  , authType = KeysAuthentication
}

autoServer :: IO Server
autoServer = do
  home <- getHomeDirectory
  return $ defaultServer {
    knownHostsPath = home </> ".ssh" </> "known_hosts"
    , publicKeyPath = home </> ".ssh" </> "id_rsa.pub"
    , privateKeyPath = home </> ".ssh" </> "id_rsa"
  }
