{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cric.Core
  ( Cric
  , CricT(..)
  , Result(..)
  , FileTransferOptions(..)
  , Executor
  , runCric
  , install
  , mkExecutor
  , installOn
  , isSuccess
  , outFromResult
  , errFromResult
  , defaultFileTransferOptions
  , dfto
  ) where

import           Control.Applicative
import           Control.Monad.Trans

import           Data.ByteString.Lazy.Char8   ()
import           Data.List
import           Data.String.Utils            (replace)
import qualified Data.ByteString.Char8        as BS

import qualified Network.SSH.Client.SimpleSSH as SSH

import           Cric.MonadCric
import           Cric.TypeDefs

-- | Monad transformer in the heart of Cric.
newtype CricT m a = CricT
  { runCricT :: forall s. SshSession s
             => Logger m
             -> Context
             -> Server
             -> Executor s m
             -> m a
  }

instance Monad m => Monad (CricT m) where
  return x = CricT $ \_ _ _ _ -> return x
  cric >>= f = CricT $ \logger context server executor -> do
    x <- runCricT cric logger context server executor
    runCricT (f x) logger context server executor
  fail msg = CricT $ fail msg

instance MonadTrans CricT where
  lift m = CricT $ \_ _ _ _ -> m

instance MonadIO m => MonadIO (CricT m) where
  liftIO = lift . liftIO

type Cric a = CricT IO a

instance Monad m => MonadCric (CricT m) where
  exec               = execCricT
  sendFile           = sendFileCricT
  withChangedContext = withChangedContextCricT
  logMsg             = logMsgCricT
  getContext         = getContextCricT
  getServer          = getServerCricT

instance Functor f => Functor (CricT f) where
  fmap f cric = CricT $ \logger context server executor ->
    fmap f $ runCricT cric logger context server executor

instance Applicative f => Applicative (CricT f) where
  pure val = CricT $ \_ _ _ _ -> pure val
  cff <*> cfa = CricT $ \logger context server executor ->
    let ff = runCricT cff logger context server executor
        fa = runCricT cfa logger context server executor
    in ff <*> fa

-- | Alias to 'runCricT', limiting its use with IO.
runCric :: Cric a -> (forall s. SshSession s => Logger IO -> Context -> Server -> Executor s IO -> IO a)
runCric = runCricT

-- | Connect to a server and run an installation (\"CricT m\" action) with simplessh.
install :: MonadIO m
        => CricT m a -- ^ What to do
        -> Logger m  -- ^ Logger
        -> Context   -- ^ Execution context
        -> Server    -- ^ Connection information
        -> m a
install cric logger context server = runCricT cric logger context server $ mkExecutor server

-- | Make an 'Executor' from a 'Server' using simplessh. It can be used with 'runCricT'.
mkExecutor :: MonadIO m => Server -> Executor SSH.Session m
mkExecutor server f = do
  let action = case authType server of
        KeysAuthentication ->
          SSH.withSessionKey
            (hostname server)
            (toInteger $ port server)
            (knownHostsPath server)
            (user server)
            (publicKeyPath server)
            (privateKeyPath server)
            (passphrase server)
            f
        PasswordAuthentication ->
          SSH.withSessionPassword
            (hostname server)
            (toInteger $ port server)
            (knownHostsPath server)
            (user server)
            (password server)
            f
  eRes <- liftIO $ SSH.runSimpleSSH action
  return $ case eRes of
    Left err  -> Left $ show err
    Right res -> Right res

-- | Connect to a server and run an installation with the default logger and context.
installOn :: MonadIO m => CricT m a -> Server -> m a
installOn cric server = install cric stdoutLogger defaultContext server

execCricT :: Monad m => String -> CricT m Result
execCricT cmd = do
    context <- getContextCricT
    let cmd' = addUser context . addDir context . addEnv (currentEnv context) $ cmd
    logMsg Debug $ "Executing " ++ cmd' ++ " ..."
    res <- exec' cmd'
    let out = case outFromResult res of
                ""   -> "No output."
                out' -> out'
        err = case errFromResult res of
                ""   -> "No output."
                err' -> err'
    logMsg Debug $ "Executed: " ++ cmd' ++ "\n"
                ++ "Stdout: " ++ BS.unpack out ++ "\n"
                ++ "Stderr: " ++ BS.unpack err
    return res
  where
    exec' cmdWithContext = CricT $ \_ _ _ executor -> do
      eRes <- executor $ \session -> sshExecCommand session cmdWithContext
      return $ case eRes of
        Left err -> SSHError err
        Right (exit, out, err) -> case exit of
          Left 0    -> Success out err
          Left code -> Failure code out err
          Right sig -> Interrupted sig out err

    addEnv [] c = c
    addEnv ((name,value):rest) c = addEnv rest $ "export " ++ name ++ "=" ++ value ++ "; " ++ c
    addDir context c = case currentDir context of
      "" -> c
      d  -> "cd " ++ d ++ "; " ++ c
    addUser context c = case currentUser context of
      "" -> c
      u  -> "sudo su " ++ u ++ " -c \'" ++ replace "\'" "\\\'" c ++ "\'"

-- returns either the number of bytes tranferred
-- or True/False whether the transfer has been successful
-- if the md5 command is available
sendFileCricT :: Monad m => FilePath -> FilePath -> FileTransferOptions -> CricT m (Maybe String)
sendFileCricT from to options = do
    logMsgCricT Info $ "Transferring file from `" ++ from ++ "` to `" ++ to ++ "` ..."
    -- TOO: That's a bit nasty
    eRes <- CricT $ \_ _ _ executor -> executor $ \session -> do
      sshSendFile session (permissions options) from to
    case eRes :: Either String Integer of
      Right bytes -> do
        logMsgCricT Info $ show bytes ++ " bytes transferred."
        case md5Hash options of
          Nothing -> return Nothing
          Just hash -> do
            mCheck <- testHash hash [("md5", ("md5 -q "++)), ("md5sum", ("md5sum "++))]
            return $ mCheck >>= (\check -> if check then Nothing else Just "MD5 mismatch")
      Left err -> return $ Just $ "SSH Error: " ++ err
  where
    testHash :: Monad m => String -> [(String, FilePath -> String)] -> CricT m (Maybe Bool)
    testHash _ [] = return Nothing
    testHash hash ((cmdName, toCmd):rest) = do
      testCmd <- testCommand cmdName
      if testCmd
        then do
          res <- execCricT $ toCmd to
          return . Just . (hash `isPrefixOf`) . BS.unpack $ case res of
            Success bs _       -> bs
            Failure _ bs _     -> bs
            Interrupted _ bs _ -> bs
            SSHError _         -> ""
        else
          testHash hash rest

    testCommand :: Monad m => String -> CricT m Bool
    testCommand cmd = do
      res <- execCricT $ "which " ++ cmd
      return $ isSuccess res

getServerCricT :: Monad m => CricT m Server
getServerCricT = CricT $ \_ _ server _ -> return server

getContextCricT :: Monad m => CricT m Context
getContextCricT = CricT $ \_ context _ _ -> return context

logMsgCricT :: Monad m => LogLevel -> String -> CricT m ()
logMsgCricT lvl msg = CricT $ \logger _ _ _ -> logger lvl msg

withChangedContextCricT :: (Context -> Context) -> CricT m a -> CricT m a
withChangedContextCricT f cric = CricT $ \logger context server session ->
  let context' = f context
  in runCricT cric logger context' server session
