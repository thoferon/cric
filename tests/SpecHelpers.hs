{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module SpecHelpers where

import Cric.Core
import Cric.TypeDefs

import Test.Hspec

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import System.Directory
import System.IO
import System.IO.Temp

import Control.Monad
import Control.Monad.Trans

type ExecCommandMock = String -> Maybe (IO (Int, BS.ByteString))
type SendFileMock    = Int -> FilePath -> FilePath -> Maybe (IO Integer)

data SshMock = SshMock { execCommandMocks :: [ExecCommandMock]
                       , sendFileMocks    :: [SendFileMock] }

instance Read LogLevel where
  readsPrec _ str
    | "LDebug"   `isPrefixOf` str = [(LDebug,   drop 6 str)]
    | "LInfo"    `isPrefixOf` str = [(LInfo,    drop 5 str)]
    | "LNotice"  `isPrefixOf` str = [(LNotice,  drop 7 str)]
    | "LWarning" `isPrefixOf` str = [(LWarning, drop 8 str)]
    | "LError"   `isPrefixOf` str = [(LError,   drop 6 str)]
    | "LPanic"   `isPrefixOf` str = [(LPanic,   drop 6 str)]
    | otherwise = []

instance SshSession SshMock where
  sshExecCommand (execCommandMocks -> fs) cmd =
      foldl' (\acc f -> fromMaybe acc $ f cmd) (dfl cmd) fs
    where dfl cmd = return (42, "no mock found: " <> BS.pack cmd)
  sshSendFile (sendFileMocks -> fs) perm from to =
    foldl' (\acc f -> fromMaybe acc $ f perm from to) (return 0) fs

defaultSshMock :: SshMock
defaultSshMock = SshMock [] []

mockCommand :: String -> (Int, BS.ByteString) -> SshMock -> SshMock
mockCommand cmd result sshMock =
  let mock receivedCmd = if cmd `isInfixOf` receivedCmd then Just $ return result else Nothing
  in sshMock { execCommandMocks = mock : execCommandMocks sshMock  }

testInstall :: Cric a -> Logger IO -> Context -> Server -> IO a
testInstall = testInstallWith defaultSshMock

testInstallWith :: SshSession s => s -> Cric a -> Logger IO -> Context -> Server -> IO a
testInstallWith mock cric logger context server = runCricT cric logger context server executor
  where executor f = f mock

testCric :: Cric a -> IO a
testCric cric = liftIO $ testInstall cric (\_ _ -> return ()) defaultContext defaultServer

testCricWith :: SshSession s => s -> Cric a -> IO a
testCricWith mock cric = liftIO $ testInstallWith mock cric (\_ _ -> return ()) defaultContext defaultServer

-- it will not automatically closed the handles (good enough for tests, at least for now)
-- reading the logs will though
testLogger :: IO (IO [(LogLevel, String)], Logger IO)
testLogger = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "cric-log-.test"
  return (readLogs handle, makeLogger handle)
  where
    readLogs :: Handle -> IO [(LogLevel, String)]
    readLogs handle = do
      hSeek handle AbsoluteSeek 0
      contents <- hGetContents handle
      return $ map read (lines contents)

    makeLogger :: Handle -> LogLevel -> String -> IO ()
    makeLogger handle lvl msg = hPrint handle (lvl, msg)
