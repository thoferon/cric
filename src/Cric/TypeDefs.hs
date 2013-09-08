{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Control.Monad.Trans

import qualified Data.ByteString.Char8 as BS

import System.Directory
import System.FilePath

import qualified Network.SSH.Client.SimpleSSH as SSH

-- For tests, see tests/SpecHelpers.hs
class SshSession s where
  sshExecCommand :: s -> String -> IO (Int, BS.ByteString)
  sshSendFile    :: s -> Int -> FilePath -> FilePath -> IO Integer

instance SshSession SSH.Session where
  sshExecCommand session cmd = do
    eRes <- SSH.runSimpleSSH $ SSH.execCommand session cmd
    case eRes of
      Left err  -> error $ "FIXME: Cric should bubble the errors up, received: " ++ show err
      Right res -> return (fromInteger $ SSH.exitCode res, SSH.content res)
  sshSendFile = undefined

data LogLevel = LDebug | LInfo | LNotice | LWarning | LError | LPanic
              deriving (Show, Eq)

type Logger m = LogLevel -> String -> m ()

stdoutLogger :: MonadIO m => Logger m
stdoutLogger LDebug   _   = liftIO $ return ()
stdoutLogger LInfo    msg = liftIO . putStrLn $ "[Info] "    ++ msg
stdoutLogger LNotice  msg = liftIO . putStrLn $ "[Notice] "  ++ msg
stdoutLogger LWarning msg = liftIO . putStrLn $ "[Warning] " ++ msg
stdoutLogger LError   msg = liftIO . putStrLn $ "[Error] "   ++ msg
stdoutLogger LPanic   msg = liftIO . putStrLn $ "[Panic] "   ++ msg

debugLogger :: MonadIO m => Logger m
debugLogger LDebug   msg = liftIO . putStrLn $ "[Debug] "   ++ msg
debugLogger LInfo    msg = liftIO . putStrLn $ "[Info] "    ++ msg
debugLogger LNotice  msg = liftIO . putStrLn $ "[Notice] "  ++ msg
debugLogger LWarning msg = liftIO . putStrLn $ "[Warning] " ++ msg
debugLogger LError   msg = liftIO . putStrLn $ "[Error] "   ++ msg
debugLogger LPanic   msg = liftIO . putStrLn $ "[Panic] "   ++ msg

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
