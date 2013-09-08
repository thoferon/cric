{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cric.Core (
  Cric
  , CricT(..)
  , Result(..)
  , FileTransferOptions(..)
  , Executor
  , runCric
  , install
  , installOn
  , exec
  , run
  , isSuccess
  , outputFromResult
  , defaultFileTransferOptions
  , dfto
  , sendFile
  , getServer
  , getContext
  , log
  , testCommand
  , withChangedContext
  , asUser
  , inDir
  , withEnv
  ) where

import Prelude hiding (log)

import Cric.TypeDefs

import qualified Data.ByteString.Char8 as BS

import Data.Default
import Data.List (isPrefixOf, partition)
import Data.String.Utils (replace)
import Control.Monad (liftM)
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 () -- For OverloadedStrings

import qualified Network.SSH.Client.SimpleSSH as SSH

data Result = Success BS.ByteString
            | Error Int BS.ByteString
            deriving (Show, Eq)

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess _ = False

outputFromResult :: Result -> BS.ByteString
outputFromResult (Success output) = output
outputFromResult (Error _ output) = output

data FileTransferOptions = FileTransferOptions {
  permissions :: Int
  , md5Hash :: Maybe String
} deriving Show

type Executor s m = forall a. (s -> IO a) -> m a

newtype CricT m a = CricT {
    runCricT :: forall s. SshSession s
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

runCric :: Cric a -> (forall s. SshSession s => Logger IO -> Context -> Server -> Executor s IO -> IO a)
runCric = runCricT

install :: MonadIO m => CricT m a -> Logger m -> Context -> Server -> m a
install cric logger context server = runCricT cric logger context server executor
  where
    executor :: MonadIO m => Executor SSH.Session m
    executor f = liftIO $ do
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
                (liftIO . f)
            PasswordAuthentication ->
              SSH.withSessionPassword
                (hostname server)
                (toInteger $ port server)
                (knownHostsPath server)
                (user server)
                (password server)
                (liftIO . f)
      eRes <- SSH.runSimpleSSH action
      case eRes of
        Left err  -> fail $ show err -- Can be caught by the user if your monad is a MonadError
        Right res -> return res

installOn :: MonadIO m => CricT m a -> Server -> m a
installOn cric server = install cric stdoutLogger defaultContext server

exec :: Monad m => String -> CricT m Result
exec cmd = do
    context <- getContext
    let cmd' = addUser context . addDir context . addEnv (currentEnv context) $ cmd
    log LDebug $ "Executing " ++ cmd' ++ " ..."
    res <- exec' cmd'
    let output = case res of
                   Success resp | resp == BS.empty -> "No output."
                                | otherwise        -> resp
                   Error _ resp | resp == BS.empty -> "No output."
                                | otherwise        -> resp
    log LDebug $ "Executed: " ++ cmd' ++ "\nOutput: " ++ (BS.unpack output)
    return res
  where
    exec' cmdWithContext = CricT $ \_ _ _ executor -> do
      (code, output) <- executor $ \session -> sshExecCommand session cmdWithContext
      return $ if code == 0 then
        Success output
        else
        Error code output

    addEnv [] c = c
    addEnv ((name,value):rest) c = addEnv rest $ "export " ++ name ++ "=" ++ value ++ "; " ++ c
    addDir context c = case currentDir context of
      "" -> c
      d  -> "cd " ++ d ++ "; " ++ c
    addUser context c = case currentUser context of
      "" -> c
      u  -> "sudo su " ++ u ++ " -c \'" ++ replace "\'" "\\\'" c ++ "\'"

run :: Monad m => String -> CricT m BS.ByteString
run cmd = outputFromResult `liftM` exec cmd

defaultFileTransferOptions :: FileTransferOptions
defaultFileTransferOptions = FileTransferOptions {
  permissions = 0o644
  , md5Hash = Nothing
}

dfto :: FileTransferOptions
dfto = defaultFileTransferOptions

instance Default FileTransferOptions where
  def = dfto

-- returns either the number of bytes tranferred
-- or True/False whether the transfer has been successful
-- if the md5 command is available
sendFile :: Monad m => FilePath -> FilePath -> FileTransferOptions -> CricT m (Either Integer Bool)
sendFile from to options = do
    log LInfo $ "Transferring file from `" ++ from ++ "` to `" ++ to ++ "` ..."
    bytes <- CricT $ \_ _ _ executor -> do
      executor $ \session -> sshSendFile session (permissions options) from to
    log LInfo $ show bytes ++ " bytes transferred."

    let lbytes = return $ Left bytes
    case md5Hash options of
      Just hash -> testHash hash [("md5", ("md5 -q "++)), ("md5sum", ("md5sum "++))] lbytes
      Nothing -> lbytes

  where
    testHash :: Monad m => String -> [(String, FilePath -> String)] -> CricT m (Either Integer Bool) -> CricT m (Either Integer Bool)
    testHash _ [] elseCric = elseCric
    testHash hash ((cmdName, toCmd):rest) elseCric = do
      testCmd <- testCommand cmdName
      if testCmd then
          run (toCmd to) >>= return . Right . (hash `isPrefixOf`) . BS.unpack
        else
          testHash hash rest elseCric

getServer :: Monad m => CricT m Server
getServer = CricT $ \_ _ server _ -> return server

getContext :: Monad m => CricT m Context
getContext = CricT $ \_ context _ _ -> return context

log :: Monad m => LogLevel -> String -> CricT m ()
log lvl msg = CricT $ \logger _ _ _ -> logger lvl msg

testCommand :: Monad m => String -> CricT m Bool
testCommand cmd = do
  res <- exec $ "which " ++ cmd
  return $ case res of
    Success _ -> True
    Error _ _ -> False

withChangedContext :: (Context -> Context) -> CricT m a -> CricT m a
withChangedContext f cric = CricT $ \logger context server session ->
  let context' = f context
  in runCricT cric logger context' server session

asUser :: Monad m => String -> CricT m a -> CricT m a
asUser u cric = do
    testResult <- asUser' $ exec "echo test if we can log in"
    case testResult of
      Success _ -> return ()
      Error _ _ -> log LWarning $ "Can't execute a command as user " ++ u
    asUser' cric
  where
    asUser' = withChangedContext $ \context -> context { currentUser = u }

inDir :: Monad m => FilePath -> CricT m a -> CricT m a
inDir d cric = do
    testResult <- inDir' $ exec "ls"
    case testResult of
      Success _ -> return ()
      Error _ _ -> log LWarning $ "Can't change directory to " ++ d
    inDir' cric
  where
    inDir' = withChangedContext $ \context -> context { currentDir = d }

withEnv :: [(String,String)] -> CricT m a -> CricT m a
withEnv e = withChangedContext $ \context -> context { currentEnv = mergeEnv (currentEnv context) e }
  where
    mergeEnv old [] = old
    mergeEnv old (ev@(n,_):evs) = let (_,cleanOld) = partition ((==n) . fst) old
                                  in mergeEnv (ev : cleanOld) evs
