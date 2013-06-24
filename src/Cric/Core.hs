{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cric.Core (
  Cric(..)
  , Result(..)
  , FileTransferOptions(..)
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
import Control.Monad.Trans (MonadIO(..))
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

newtype Cric a = Cric { runCric :: forall s. SshSession s => Logger -> Context -> Server -> s -> IO a }

instance Monad Cric where
  return x = Cric $ \_ _ _ _ -> return x
  cric >>= f = Cric $ \logger context server session -> do
    x <- runCric cric logger context server session
    runCric (f x) logger context server session
  fail msg = Cric $ fail msg

instance MonadIO Cric where
  liftIO ioAction = Cric $ \_ _ _ _ -> ioAction

install :: Cric a -> Logger -> Context -> Server -> IO a
install cric logger context server =
  case authType server of
    KeysAuthentication ->
      withSSH2 (knownHostsPath server)
               (publicKeyPath server)
               (privateKeyPath server)
               (passphrase server)
               (user server)
               (hostname server)
               (port server)
               (runCric cric logger context server)
    PasswordAuthentication ->
      fail "PasswordAuthentication has been disabled (see README.md for more information)"
--      withSSH2User (knownHostsPath server)
--                   (user server)
--                   (password server)
--                   (hostname server)
--                   (port server)
--                   (runCric cric logger context server)

installOn :: Cric a -> Server -> IO a
installOn cric server = install cric stdoutLogger defaultContext server

exec :: String -> Cric Result
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
    exec' cmdWithContext = Cric $ \_ _ _ session -> do
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

run :: String -> Cric BL.ByteString
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
sendFile :: FilePath -> FilePath -> FileTransferOptions -> Cric (Either Integer Bool)
sendFile from to options = do
    log LInfo $ "Transferring file from `" ++ from ++ "` to `" ++ to ++ "` ..."
    bytes <- Cric $ \_ _ _ session ->
      sshSendFile session (permissions options) from to
    log LInfo $ show bytes ++ " bytes transferred."

    let lbytes = return $ Left bytes
    case md5Hash options of
      Just hash -> testHash hash [("md5", ("md5 -q "++)), ("md5sum", ("md5sum "++))] lbytes
      Nothing -> lbytes

  where
    testHash :: String -> [(String, FilePath -> String)] -> Cric (Either Integer Bool) -> Cric (Either Integer Bool)
    testHash _ [] elseCric = elseCric
    testHash hash ((cmdName, toCmd):rest) elseCric = do
      testCmd <- testCommand cmdName
      if testCmd then
          run (toCmd to) >>= return . Right . (hash `isPrefixOf`) . TL.unpack . TL.stripEnd . TLE.decodeUtf8
        else
          testHash hash rest elseCric

getServer :: Cric Server
getServer = Cric $ \_ _ server _ -> return server

getContext :: Cric Context
getContext = Cric $ \_ context _ _ -> return context

log :: LogLevel -> String -> Cric ()
log lvl msg = Cric $ \logger _ _ _ -> logger lvl msg

testCommand :: String -> Cric Bool
testCommand cmd = do
  res <- exec $ "which " ++ cmd
  return $ case res of
    Success _ -> True
    Error _ _ -> False

withChangedContext :: (Context -> Context) -> Cric a -> Cric a
withChangedContext f cric = Cric $ \logger context server session ->
  let context' = f context
  in runCric cric logger context' server session

asUser :: String -> Cric a -> Cric a
asUser u cric = do
    testResult <- asUser' $ exec "echo test if we can log in"
    case testResult of
      Success _ -> return ()
      Error _ _ -> log LWarning $ "Can't execute a command as user " ++ u
    asUser' cric
  where
    asUser' = withChangedContext $ \context -> context { currentUser = u }

inDir :: FilePath -> Cric a -> Cric a
inDir d cric = do
    testResult <- inDir' $ exec "ls"
    case testResult of
      Success _ -> return ()
      Error _ _ -> log LWarning $ "Can't change directory to " ++ d
    inDir' cric
  where
    inDir' = withChangedContext $ \context -> context { currentDir = d }

withEnv :: [(String,String)] -> Cric a -> Cric a
withEnv e = withChangedContext $ \context -> context { currentEnv = mergeEnv (currentEnv context) e }
  where
    mergeEnv old [] = old
    mergeEnv old (ev@(n,_):evs) = let (_,cleanOld) = partition ((==n) . fst) old
                                  in mergeEnv (ev : cleanOld) evs
