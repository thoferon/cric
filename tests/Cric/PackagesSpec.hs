{-# LANGUAGE OverloadedStrings #-}

module Cric.PackagesSpec (
  test
  ) where

import Test.Hspec
import SpecHelpers

import Cric
import Cric.Packages

test :: Spec
test = do
  describe "getPackageManager" $ do
    it "detects RPM" $ do
      let sshMock = mockCommand "which rpm" (0, "/bin/rpm") defaultSshMock
      result <- testCricWith sshMock getPackageManager
      result `shouldBe` RPM

    it "detects Yum" $ do
      let sshMock = mockCommand "which yum" (0, "/bin/yum") defaultSshMock
      result <- testCricWith sshMock getPackageManager
      result `shouldBe` Yum

    it "detects APT" $ do
      let sshMock = mockCommand "which apt-get" (0, "/bin/apt-get") defaultSshMock
      result <- testCricWith sshMock getPackageManager
      result `shouldBe` APT

    it "returns UnknownPackageManager if it can't find anything" $ do
      result <- testCric getPackageManager
      result `shouldBe` UnknownPackageManager

  describe "installPackage" $ do
    it "uses the package manager found" $ do
      let mock = mockCommand "which apt-get"   (0, "/bin/apt-get")
               . mockCommand "apt-get install" (0, "apt-get called")
               $ defaultSshMock
      result <- testCricWith mock $ installPackage ("haskell-platform" :: String)
      result `shouldBe` Right "apt-get called"

    context "when it can't find a package manager" $ do
      it "returns a NoPackageManagerFound error" $ do
        result <- testCric $ installPackage ("haskell-platform" :: String)
        result `shouldBe` Left NoPackageManagerFound

    context "when the installation fails" $ do
      it "returns an error" $ do
        let mock = mockCommand "which rpm" (0, "/bin/rpm")
                 . mockCommand "rpm -i"    (1, "installation failed")
                 $ defaultSshMock

        result <- testCricWith mock $ installPackage ("haskell-platform" :: String)
        result `shouldBe`
          Left (UnknownPkgManagerError $ Failure 1 "installation failed" "")

  describe "removePackage" $ do
    it "uses the package manager found" $ do
      let mock = mockCommand "which apt-get"  (0, "/bin/apt-get")
               . mockCommand "apt-get remove" (0, "apt-get called")
               $ defaultSshMock
      result <- testCricWith mock $ removePackage ("haskell-platform" :: String)
      result `shouldBe` Right "apt-get called"

    context "when it can't find a package manager" $ do
      it "returns a NoPackageManagerFound error" $ do
        result <- testCric $ removePackage ("haskell-platform" :: String)
        result `shouldBe` Left NoPackageManagerFound

    context "when the removal fails" $ do
      it "returns an error" $ do
        let mock = mockCommand "which rpm" (0, "/bin/rpm")
                 . mockCommand "rpm -e"    (1, "removal failed")
                 $ defaultSshMock

        result <- testCricWith mock $ removePackage ("haskell-platform" :: String)
        result `shouldBe`
          Left (UnknownPkgManagerError $ Failure 1 "removal failed" "")
