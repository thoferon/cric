{-# LANGUAGE OverloadedStrings #-}

module Cric.SystemInfoSpec
  ( test
  ) where

import Control.Monad.Trans

import Data.List

import Test.Hspec
import SpecHelpers

import Cric
import Cric.SystemInfo

testGetOS resp exp = do
  let mock = mockCommand "uname -o" resp defaultSshMock
  result <- testCricWith mock getOS
  result `shouldBe` exp

test :: Spec
test = do
  describe "getOS" $ do
    it "detects Linux"   $ testGetOS (0, "GNU/Linux") Linux
    it "detects FreeBSD" $ testGetOS (0, "FreeBSD")   FreeBSD

    it "returns Unknown with the value if not found" $ do
      testGetOS (0, "unrecognized") $ UnknownOS "unrecognized"

  describe "testCommand" $ do
    let cric = testCommand "cmd"
    let testMock resp cmd = if "which cmd" `isInfixOf` cmd
          then Just $ return resp
          else Nothing

    it "returns True if the command exists" $ do
      let sshMock = SshMock [testMock (0, "/bin/cmd")] []
      result <- liftIO $ testCricWith sshMock cric
      result `shouldBe` True

    it "returns False if the command doesn't exist" $ do
      let sshMock = SshMock [testMock (1, "")] []
      result <- liftIO $ testCricWith sshMock cric
      result `shouldBe` False
