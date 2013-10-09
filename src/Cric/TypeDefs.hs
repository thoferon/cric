{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Cric.TypeDefs
  ( Server(..)
  , Result(..)
  , FileTransferOptions(..)
  , AuthenticationType(..)
  , Executor
  , Logger
  , LogLevel(..)
  , Context(..)
  , SshSession(..)
  , isSuccess
  , outputFromResult
  , defaultServer
  , defaultContext
  , defaultFileTransferOptions
  , dfto
  , stdoutLogger
  , debugLogger
  , autoServer
  ) where

import           Control.Monad.Trans

import           Data.Default
import qualified Data.ByteString.Char8        as BS

import           System.Directory
import           System.FilePath

import qualified Network.SSH.Client.SimpleSSH as SSH

data Result = Success BS.ByteString
            | Failure Int BS.ByteString
            deriving (Show, Eq)

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess _           = False

outputFromResult :: Result -> BS.ByteString
outputFromResult res = case res of
  Success output   -> output
  Failure _ output -> output

data FileTransferOptions = FileTransferOptions
  { permissions :: Int
  , md5Hash     :: Maybe String
  } deriving Show

defaultFileTransferOptions :: FileTransferOptions
defaultFileTransferOptions = FileTransferOptions {
  permissions = 0o644
  , md5Hash = Nothing
}

dfto :: FileTransferOptions
dfto = defaultFileTransferOptions

instance Default FileTransferOptions where
  def = dfto

type Executor s m = forall a. (s -> IO a) -> m a

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
  sshSendFile session mode source target = do
    eRes <- SSH.runSimpleSSH $ SSH.sendFile session (toInteger mode) source target
    case eRes of
      Left err  -> error $ "FIXME: Cric should bubble the errors up, received: " ++ show err
      Right res -> return res

data LogLevel = Debug | Info | Notice | Warning | Error | Panic deriving (Show, Eq)

type Logger m = LogLevel -> String -> m ()

stdoutLogger :: MonadIO m => Logger m
stdoutLogger Debug   _   = liftIO $ return ()
stdoutLogger Info    msg = liftIO . putStrLn $ "[Info] "    ++ msg
stdoutLogger Notice  msg = liftIO . putStrLn $ "[Notice] "  ++ msg
stdoutLogger Warning msg = liftIO . putStrLn $ "[Warning] " ++ msg
stdoutLogger Error   msg = liftIO . putStrLn $ "[Error] "   ++ msg
stdoutLogger Panic   msg = liftIO . putStrLn $ "[Panic] "   ++ msg

debugLogger :: MonadIO m => Logger m
debugLogger Debug msg = liftIO . putStrLn $ "[Debug] "   ++ msg
debugLogger lvl   msg = stdoutLogger lvl msg

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
