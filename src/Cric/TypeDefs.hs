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

-- | Structure representing the result of a command execution.
data Result = Success BS.ByteString     -- ^ Success with what was printed to stdout
            | Failure Int BS.ByteString -- ^ Failure with the status code and stdout
            deriving (Show, Eq)

-- | Helper function to test for success.
isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess _           = False

-- | Extract the output of a command execution.
outputFromResult :: Result -> BS.ByteString
outputFromResult res = case res of
  Success output   -> output
  Failure _ output -> output

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

instance Default FileTransferOptions where
  def = dfto

-- | Way to execute the SSH command into another monad.
type Executor s m = forall a. (s -> IO a) -> m a

-- | Typeclass used as an interface between Cric and SSH.
--
-- It is the bridge with simplessh and, for tests, to mocks.
class SshSession s where
  -- | Execute a command on an SSH session.
  sshExecCommand :: s                       -- ^ Session
                 -> String                  -- ^ Command
                 -> IO (Int, BS.ByteString) -- ^ Status code & output
  -- | Send a file through SSH.
  sshSendFile    :: s          -- ^ Session
                 -> Int        -- ^ File mode
                 -> FilePath   -- ^ Local path
                 -> FilePath   -- ^ Remote path
                 -> IO Integer -- ^ Bytes transferred

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

-- | The different levels used by the logger.
data LogLevel = Debug | Info | Notice | Warning | Error | Panic deriving (Show, Eq)

-- | A logger in a monad, e.g. 'CricT' IO.
type Logger m = LogLevel -> String -> m ()

-- | Simple logger to stdout, ignoring log level 'Debug'.
stdoutLogger :: MonadIO m => Logger m
stdoutLogger Debug   _   = liftIO $ return ()
stdoutLogger Info    msg = liftIO . putStrLn $ "[Info] "    ++ msg
stdoutLogger Notice  msg = liftIO . putStrLn $ "[Notice] "  ++ msg
stdoutLogger Warning msg = liftIO . putStrLn $ "[Warning] " ++ msg
stdoutLogger Error   msg = liftIO . putStrLn $ "[Error] "   ++ msg
stdoutLogger Panic   msg = liftIO . putStrLn $ "[Panic] "   ++ msg

-- | Same as 'stdoutLogger' but displaying log level 'Debug'.
debugLogger :: MonadIO m => Logger m
debugLogger Debug msg = liftIO . putStrLn $ "[Debug] "   ++ msg
debugLogger lvl   msg = stdoutLogger lvl msg

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
