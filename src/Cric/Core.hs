{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cric.Core (
  Cric
  , CricT(..)
  , Result(..)
  , FileTransferOptions(..)
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

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.List (isPrefixOf, partition)
import Data.String.Utils (replace)
import Control.Monad (liftM)
import Control.Monad.Trans
import Network.SSH.Client.LibSSH2
import Data.ByteString.Lazy.Char8 () -- For OverloadedStrings

data Result = Success BL.ByteString
            | Error Int BL.ByteString
            deriving (Show, Eq)

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess _ = False

outputFromResult :: Result -> BL.ByteString
outputFromResult (Success output) = output
outputFromResult (Error _ output) = output

data FileTransferOptions = FileTransferOptions {
  permissions :: Int
  , md5Hash :: Maybe String
} deriving Show

newtype CricT m a = CricT {
    runCricT :: forall s. SshSession m s
             => Logger m
             -> Context
             -> Server
             -> m s
             -> m a
  }

instance Monad m => Monad (CricT m) where
  return x = CricT $ \_ _ _ _ -> return x
  cric >>= f = CricT $ \logger context server connect -> do
    x <- runCricT cric logger context server connect
    runCricT (f x) logger context server connect
  fail msg = CricT $ fail msg

instance MonadTrans CricT where
  lift m = CricT $ \_ _ _ _ -> m

instance MonadIO m => MonadIO (CricT m) where
  liftIO = lift . liftIO

type Cric a = CricT IO a

runCric :: Cric a -> (forall s. SshSession IO s => Logger IO -> Context -> Server -> IO s -> IO a)
runCric = runCricT

install :: MonadIO m => CricT m a -> Logger m -> Context -> Server -> m a
install cric logger context server = runCricT cric logger context server connect
  where
    connect :: MonadIO m => m Session
    connect = liftIO $ case authType server of
                KeysAuthentication ->
                  withSSH2 (knownHostsPath server)
                           (publicKeyPath server)
                           (privateKeyPath server)
                           (passphrase server)
                           (user server)
                           (hostname server)
                           (port server)
                           return
                PasswordAuthentication ->
                  fail "PasswordAuthentication has been disabled (see README.md for more information)"
--                 withSSH2User (knownHostsPath server)
--                              (user server)
--                              (password server)
--                              (hostname server)
--                              (port server)
--                              (runCric cric logger context server)

installOn :: MonadIO m => CricT m a -> Server -> m a
installOn cric server = install cric stdoutLogger defaultContext server

exec :: Monad m => String -> CricT m Result
exec cmd = do
    context <- getContext
    let cmd' = addDir context . addEnv (currentEnv context) . addUser context $ cmd
    log LDebug $ "Executing " ++ cmd' ++ " ..."
    res <- exec' cmd'
    let output = case res of
                   Success resp    | resp == BL.empty -> "No output."
                                   | otherwise        -> resp
                   Error _ resp    | resp == BL.empty -> "No output."
                                   | otherwise        -> resp
    log LDebug $ "Executed: " ++ cmd' ++ "\nOutput: " ++ (TL.unpack . TL.stripEnd . TLE.decodeUtf8 $ output)
    return res
  where
    exec' cmdWithContext = CricT $ \_ _ _ connect -> do
      session <- connect
      (code, outputs) <- sshExecCommands session [cmdWithContext]
      let output = BL.concat outputs
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
      u  -> "su " ++ u ++ " -c \"" ++ replace "\"" "\\\"" c ++ "\""

run :: Monad m => String -> CricT m BL.ByteString
run cmd = outputFromResult `liftM` exec cmd

defaultFileTransferOptions :: FileTransferOptions
defaultFileTransferOptions = FileTransferOptions {
  permissions = 0o644
  , md5Hash = Nothing
}

dfto :: FileTransferOptions
dfto = defaultFileTransferOptions

-- returns either the number of bytes tranferred
-- or True/False whether the transfer has been successful
-- if the md5 command is available
sendFile :: Monad m => FilePath -> FilePath -> FileTransferOptions -> CricT m (Either Integer Bool)
sendFile from to options = do
    log LInfo $ "Transferring file from `" ++ from ++ "` to `" ++ to ++ "` ..."
    bytes <- CricT $ \_ _ _ connect -> do
      session <- connect
      sshSendFile session (permissions options) from to
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
          run (toCmd to) >>= return . Right . (hash `isPrefixOf`) . TL.unpack . TL.stripEnd . TLE.decodeUtf8
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
