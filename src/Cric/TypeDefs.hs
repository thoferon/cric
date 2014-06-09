{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
  , outFromResult
  , errFromResult
  , defaultServer
  , defaultContext
  , defaultFileTransferOptions
  , dfto
  , stdoutLogger
  , debugLogger
  , traceLogger
  , autoServer
  ) where

import           Control.Monad.Trans

import           Data.Default
import qualified Data.ByteString.Char8        as BS

import           System.Directory
import           System.FilePath
import           System.IO

import qualified Network.SSH.Client.SimpleSSH as SSH

-- | Structure representing the result of a command execution.
data Result
  = Success BS.ByteString BS.ByteString
  -- ^ Success with what was printed to stdout and stderr
  | Failure Integer BS.ByteString BS.ByteString
  -- ^ Failure with the status code, stdout and stderr
  | Interrupted BS.ByteString BS.ByteString BS.ByteString
  -- ^ Signal, stdout and stderr
  | SSHError String
  deriving (Show, Eq)

-- | Helper function to test for success.
isSuccess :: Result -> Bool
isSuccess (Success _ _) = True
isSuccess _             = False

-- | Extract the output of a command execution.
outFromResult :: Result -> BS.ByteString
outFromResult res = case res of
  Success out _       -> out
  Failure _ out _     -> out
  Interrupted _ out _ -> out
  SSHError _          -> ""

errFromResult :: Result -> BS.ByteString
errFromResult res = case res of
  Success _ err       -> err
  Failure _ _ err     -> err
  Interrupted _ _ err -> err
  SSHError _          -> ""

-- | Extra options to pass to 'sendFile'.
data FileTransferOptions = FileTransferOptions
  { permissions :: Int          -- ^ File mode (e.g. 0o755)
  , md5Hash     :: Maybe String -- ^ MD5 hash to check the file against
  } deriving Show

-- | Default options for 'sendFile'.
defaultFileTransferOptions :: FileTransferOptions
defaultFileTransferOptions = FileTransferOptions
  { permissions = 0o644
  , md5Hash     = Nothing
  }

-- | Alias for 'defaultFileTransferOptions'.
dfto :: FileTransferOptions
dfto = defaultFileTransferOptions

instance Default FileTransferOptions where def = dfto

-- | Way to execute the SSH command into another monad.
type Executor s m = forall a. (s -> SessionMonad s a) -> m (Either String a)

-- | Typeclass used as an interface between Cric and SSH.
--
-- It is the bridge with simplessh and, for tests, to mocks.
class SshSession s where
  -- | The monad in which the session should be used.
  type SessionMonad s a

  -- | Execute a command on an SSH session.
  sshExecCommand :: s      -- ^ Session
                 -> String -- ^ Command
                 -> SessionMonad s
                      ( Either Integer BS.ByteString
                      , BS.ByteString
                      , BS.ByteString
                      ) -- ^ Status code or signal & stdout/stderr

  -- | Send a file through SSH.
  sshSendFile :: s        -- ^ Session
              -> Int      -- ^ File mode
              -> FilePath -- ^ Local path
              -> FilePath -- ^ Remote path
              -> SessionMonad s Integer -- ^ Bytes transferred

instance SshSession SSH.Session where
  type SessionMonad SSH.Session a = SSH.SimpleSSH a

  sshExecCommand session cmd = do
    res <- SSH.execCommand session cmd
    let exit = case SSH.resultExit res of
                 SSH.ExitSuccess      -> Left 0
                 SSH.ExitFailure code -> Left code
                 SSH.ExitSignal  sig  -> Right sig
    return (exit, SSH.resultOut res, SSH.resultErr res)

  sshSendFile session mode source target = SSH.sendFile session (toInteger mode) source target

-- | The different levels used by the logger.
data LogLevel
  = Trace | Debug | Info | Notice | Warning | Error | Panic
  deriving (Show, Eq)

-- | A logger in a monad, e.g. 'CricT' IO.
type Logger m = LogLevel -> String -> m ()

-- | Simple logger to stdout, ignoring log level 'Debug'.
stdoutLogger :: MonadIO m => Logger m
stdoutLogger Trace   _   = liftIO $ return ()
stdoutLogger Debug   _   = liftIO $ return ()
stdoutLogger Info    msg = liftIO . hPutStrLn stderr $ "[Info] "    ++ msg
stdoutLogger Notice  msg = liftIO . hPutStrLn stderr $ "[Notice] "  ++ msg
stdoutLogger Warning msg = liftIO . hPutStrLn stderr $ "[Warning] " ++ msg
stdoutLogger Error   msg = liftIO . hPutStrLn stderr $ "[Error] "   ++ msg
stdoutLogger Panic   msg = liftIO . hPutStrLn stderr $ "[Panic] "   ++ msg

-- | Same as 'stdoutLogger' but displaying log level 'Debug'.
debugLogger :: MonadIO m => Logger m
debugLogger Debug msg = liftIO . hPutStrLn stderr $ "[Debug] " ++ msg
debugLogger lvl   msg = stdoutLogger lvl msg

-- | Same as 'traceLogger' but displaying log level 'Trace'.
traceLogger :: MonadIO m => Logger m
traceLogger Trace msg = liftIO . hPutStrLn stderr $ "[Trace] " ++ msg
traceLogger lvl   msg = debugLogger lvl msg

-- | Authentication type, either with public keys (RSA) or by username/password.
data AuthenticationType = KeysAuthentication
                        | PasswordAuthentication
                        deriving (Show, Eq)

-- | Context of execution of commands.
data Context = Context
  { currentUser :: String             -- ^ User
  , currentDir  :: FilePath           -- ^ Working directory
  , currentEnv  :: [(String, String)] -- ^ List of pairs name/value
  } deriving (Eq, Show)

-- | Default context of execution: no context.
defaultContext :: Context
defaultContext = Context
  { currentUser = ""
  , currentDir  = ""
  , currentEnv  = []
  }

-- | Connection information to a server.
data Server = Server
  { hostname       :: String             -- ^ Host or IP address
  , port           :: Int                -- ^ Port
  , user           :: String             -- ^ Username
  , password       :: String             -- ^ Password
  , knownHostsPath :: FilePath           -- ^ Path to known hosts
  , publicKeyPath  :: FilePath           -- ^ Path to public key
  , privateKeyPath :: FilePath           -- ^ Path to private key
  , passphrase     :: String             -- ^ Passphrase for the key
  , authType       :: AuthenticationType -- ^ Type of authentication
  } deriving (Eq, Show)

-- | Default connection information.
defaultServer :: Server
defaultServer = Server
  { hostname       = "127.0.0.1"
  , port           = 22
  , user           = "root"
  , password       = ""
  , knownHostsPath = "known_hosts"
  , publicKeyPath  = "id_rsa.pub"
  , privateKeyPath = "id_rsa"
  , passphrase     = ""
  , authType       = KeysAuthentication
  }

instance Default Server where def = defaultServer

-- | Add paths to the default 'Server'.
--
-- It will resolve the home directory and set
-- ~/.ssh/known_hosts to 'knownHostsPath',
-- ~/.ssh/id_rsa.pub to 'publicKeyPath' and
-- ~/.ssh/id_rsa to 'privateKeyPath'.
autoServer :: IO Server
autoServer = do
  home <- getHomeDirectory
  return $ defaultServer
    { knownHostsPath = home </> ".ssh" </> "known_hosts"
    , publicKeyPath  = home </> ".ssh" </> "id_rsa.pub"
    , privateKeyPath = home </> ".ssh" </> "id_rsa"
    }
