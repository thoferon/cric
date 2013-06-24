{-# LANGUAGE OverloadedStrings #-}

module Cric.CoreSpec (
  test
  ) where

import Cric
import SpecHelpers
import Test.Hspec
import Control.Monad.Trans
import Data.List
import Data.ByteString.Lazy.Char8 ()

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

test :: Spec
test = do
  let server = defaultServer { hostname = "localhost" }
  let logger = \_ _ -> return ()
  let context = defaultContext

  describe "Cric" $ it "" $ pendingWith "monadic laws + MonadIO"

  describe "exec" $ do
    it "returns Success if the exit code is 0 with the outputs" $ do
      let cric = exec "echo test"
      let sshMock = SshMock [const (Just $ return (0, ["test", "2"]))] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Success "test2"

    it "returns Success if the exit code is 0 with the outputs" $ do
      let cric = exec "echo test"
      let sshMock = SshMock [const (Just $ return (1, ["test", "2"]))] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Error 1 "test2"

    it "adds the context" $ do
      let cric = exec "echo \"test\" 'test'"
      let context' = defaultContext { currentUser = "user", currentDir = "dir", currentEnv = [("a", "1")] }
      let mockFunc cmds = if "cd dir; export a=1; su user -c \"echo \\\"test\\\" 'test'\"" `elem` cmds
                          then Just $ return (0, ["successful"])
                          else Nothing
      let sshMock = SshMock [mockFunc] []
      result <- liftIO $ testInstallWith sshMock cric logger context' server
      result `shouldBe` Success "successful"

    it "logs the command" $ do
      let cric = exec "echo test"
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        testInstall cric logger' defaultContext server
        getLogs
      logs `shouldSatisfy` any (\(lvl, msg) -> lvl == LDebug && "echo test" `isInfixOf` msg)

    it "logs the output" $ do
      let cric = exec "echo test"
      let mockFunc _ = Just $ return (0, ["test output"])
      let sshMock = SshMock [mockFunc] []
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        testInstallWith sshMock cric logger' defaultContext server
        getLogs
      logs `shouldSatisfy` any (\(lvl, msg) -> lvl == LDebug && "test output" `isInfixOf` msg)

  describe "run" $ do
    it "runs the command and sends back the ouput" $ do
      let cric = exec "echo test"
      let mockFunc cmds = if any ("echo test" `isInfixOf`) cmds
                          then Just $ return (0, ["test output"])
                          else Nothing
      let sshMock = SshMock [mockFunc] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Success "test output"

  describe "sendFile" $ do
    it "sends a file with correct destination/permissions (and returns the size if no md5sum is found)" $ do
      let cric = sendFile "from" "to" dfto
      let sendMock perm from to = if (perm, from, to) == (permissions dfto, "from", "to")
                                  then Just $ return 1337
                                  else Nothing
      -- testMock is used to disable the checksum and return 1337
      let testMock cmds = Just $ return (127, [""])
      let sshMock = SshMock [testMock] [sendMock]
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Left 1337

    it "returns a boolean if a md5sum is found" $ do
      let cric = sendFile "from" "to" $ dfto { md5Hash = Just "test md5 hash" }
      let sendMock perm from to = Just $ return 1337
      let testMock cmds = if any ("which md5" `isInfixOf`) cmds
                          then Just $ return (0, ["/usr/bin/md5\n"])
                          else Nothing
      let md5Mock cmds = if any ("md5 -q to" `isInfixOf`) cmds
                        then Just $ return (0, ["test md5 hash\n"])
                        else Nothing
      let sshMock = SshMock [testMock, md5Mock] [sendMock]
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` Right True

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

  describe "log" $ do
    it "uses the logger passed" $ do
      let cric = Cric.log LWarning "blah"
      logs <- liftIO $ do
        (getLogs, logger') <- testLogger
        let logger'' lvl msg = logger' lvl ("I am the one saying " ++ msg)
        testInstall cric logger'' defaultContext server
        getLogs
      logs `shouldSatisfy` any (== (LWarning, "I am the one saying blah"))

  describe "testCommand" $ do
    let cric = testCommand "cmd"
    let testMock resp cmds = if any ("which cmd" `isInfixOf`) cmds
                             then Just $ return resp
                             else Nothing

    it "returns True if the command exists" $ do
      let sshMock = SshMock [testMock (0, ["/bin/cmd"])] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` True

    it "returns False if the command doesn't exist" $ do
      let sshMock = SshMock [testMock (1, [""])] []
      result <- liftIO $ testInstallWith sshMock cric logger context server
      result `shouldBe` False

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
