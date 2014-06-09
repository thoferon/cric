{-# LANGUAGE OverloadedStrings #-}

module Cric.CoreSpec
  ( test
  ) where

import           Control.Monad.Error
import           Control.Monad.Trans

import           Data.List
import qualified Data.ByteString.Char8 as BS

import           SpecHelpers
import           Test.Hspec

import           Cric

test :: Spec
test = do
  let server = defaultServer { hostname = "localhost" }
  let logger = \_ _ -> return ()
  let context = defaultContext

  describe "Cric" $ it "" $ pendingWith "monadic laws + MonadIO"

  describe "exec" $ do
    it "returns Success if the exit code is 0 with the outputs" $ do
      let cric = exec "echo test"
          sshMock = SshMock [const (Just $ return (Left 0, "out", "err"))] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Success "out" "err"

    it "returns Failure if the exit code is non-0 with code and outputs" $ do
      let cric = exec "echo test"
          sshMock = SshMock [const (Just $ return (Left 1, "out", "err"))] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Failure 1 "out" "err"

    it "returns Interrupted if there is a signal with signal and outputs" $ do
      let cric = exec "echo test"
          sshMock = SshMock [const (Just $ return (Right "HUP", "out", "err"))] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Interrupted "HUP" "out" "err"

    it "returns SSHError with the error message" $ do
      let cric = exec "echo test"
          sshMock = SshMock [const (Just $ throwError "some error")] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` SSHError "some error"

    it "adds the context" $ do
      let cric = exec "echo \"test\" 'test'"
          context' = defaultContext { currentUser = "user", currentDir = "dir", currentEnv = [("a", "1")] }
          mockFunc cmd = if cmd == "sudo -u user -H -c - sh -c \"cd dir; export a=1; echo \\\"test\\\" 'test'\""
                          then Just $ return (Left 0, "successful", "")
                          else Nothing
          sshMock = SshMock [mockFunc] []
      result <- liftIO $ testInstallWith sshMock cric logger context' server
      result `shouldBe` Success "successful" ""

    it "logs the command" $ do
      let cric = exec "echo test"
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        testInstall cric logger' defaultContext server
        getLogs
      logs `shouldSatisfy` any (\(lvl, msg) -> lvl == Debug && "echo test" `isInfixOf` msg)

    it "logs the output" $ do
      let cric       = exec "echo test"
          mockFunc _ = Just $ return (Left 0, "test output", "")
          sshMock    = SshMock [mockFunc] []
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        testInstallWith sshMock cric logger' defaultContext server
        getLogs
      logs `shouldSatisfy` any (\(lvl, msg) -> lvl == Debug && "test output" `isInfixOf` msg)

  describe "run" $ do
    it "runs the command and sends back the ouput" $ do
      let cric = exec "echo test"
          mockFunc cmd = if "echo test" `isInfixOf` cmd
            then Just $ return (Left 0, "test output", "")
            else Nothing
          sshMock = SshMock [mockFunc] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Success "test output" ""

  describe "sendFile" $ do
    it "sends a file with correct destination/permissions" $ do
      let cric = sendFile "from" "to" dfto
          sendMock perm from to = if (perm, from, to) == (permissions dfto, "from", "to")
                                  then Just $ return 1337
                                  else Nothing
          -- testMock is used to disable the checksum and return 1337
          testMock cmd | "md5" `isInfixOf` cmd = Just $ return (Left 127, "", "")
                       | otherwise             = Just $ return (Left 0, BS.pack cmd, "")
          sshMock = SshMock [testMock] [sendMock]
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Nothing

    it "returns a boolean if a md5sum is found" $ do
      let cric = sendFile "from" "to" $ dfto { md5Hash = Just "test md5 hash" }
          sendMock perm from to = Just $ return 1337
          testMock cmd
            | "which md5" `isInfixOf` cmd = Just $ return (Left 0, "/usr/bin/md5\n", "")
            | "mv" `isInfixOf` cmd = Just $ return (Left 0, BS.pack cmd, "")
            | otherwise = Nothing
          md5Mock cmd = if "md5 -q /tmp/from.cric-tmp" `isInfixOf` cmd
                          then Just $ return (Left 0, "test md5 hash\n", "")
                          else Nothing
          sshMock = SshMock [testMock, md5Mock] [sendMock]
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Nothing

  describe "getServer" $ do
    it "returns the server" $ do
      let cric = getServer
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` server

  describe "getContext" $ do
    it "returns the context" $ do
      let cric = getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context

  describe "logMsg" $ do
    it "uses the logger passed" $ do
      let cric = logMsg Warning "blah"
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        let logger'' lvl msg = logger' lvl ("I am the one saying " ++ msg)
        testInstall cric logger'' defaultContext server
        getLogs
      logs `shouldSatisfy` any (== (Warning, "I am the one saying blah"))


  describe "asUser" $ do
    it "sets the user in the context" $ do
      let cric = asUser "test" getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context { currentUser = "test" }

  describe "inDir" $ do
    it "sets the directory in the context" $ do
      let cric = inDir "test" getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context { currentDir = "test" }

  describe "withEnv" $ do
    it "sets environement variables in the context" $ do
      let cric = withEnv [("a", "1")] getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context { currentEnv = [("a", "1")] }

    it "keeps the old variables" $ do
      let cric = withEnv [("a", "1")] . withEnv [("b", "2")] $ getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context { currentEnv = [("b", "2"), ("a", "1")] }

    it "overwrites the old variables if a new one is found" $ do
      let cric = withEnv [("a", "1")] . withEnv [("a", "2")] $ getContext
      result <- liftIO $ testInstall cric logger context server
      result `shouldBe` context { currentEnv = [("a", "2")] }
