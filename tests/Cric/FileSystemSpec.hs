{-# LANGUAGE OverloadedStrings #-}

module Cric.FileSystemSpec
  ( test
  ) where

import           Test.Hspec
import           SpecHelpers

import           Cric
import           Cric.FileSystem

import           Data.List
import qualified Data.ByteString.Char8 as BS

test :: Spec
test = do
  describe "createFile" $ do
    it "executes touch" $ do
      result <- testCric $ createFile "filepath"
      result `shouldSatisfy` (`outputContains` "touch filepath")

  describe "createDirectory" $ do
    it "executes mkdir -p" $ do
      result <- testCric $ createDirectory "filepath"
      result `shouldSatisfy` (`outputContains` "mkdir -p filepath")

  describe "removeFile" $ do
    it "executes rm -f" $ do
      result <- testCric $ removeFile "filepath"
      result `shouldSatisfy` (`outputContains` "rm -f filepath")

  describe "removeDirectory" $ do
    it "executes rm -rf" $ do
      result <- testCric $ removeDirectory "filepath"
      result `shouldSatisfy` (`outputContains` "rm -rf filepath")

  describe "copyFile" $ do
    it "executes cp -f" $ do
      result <- testCric $ copyFile "from" "to"
      result `shouldSatisfy` (`outputContains` "cp -f from to")

  describe "copyDirectory" $ do
    it "executes cp -rf" $ do
      result <- testCric $ copyDirectory "from" "to"
      result `shouldSatisfy` (`outputContains` "cp -rf from to")

  describe "moveFile" $ do
    it "executes mv -f" $ do
      result <- testCric $ moveFile "from" "to"
      result `shouldSatisfy` (`outputContains` "mv -f from to")

  describe "moveDirectory" $ do
    it "executes mv -rf" $ do
      result <- testCric $ moveDirectory "from" "to"
      result `shouldSatisfy` (`outputContains` "mv -rf from to")

  describe "chmod" $ do
    it "changes the permission with chmod" $ do
      result <- testCric $ chmod 0o777 "filepath"
      result `shouldSatisfy` (`outputContains` "chmod 777 filepath")

  describe "chmodRec" $ do
    it "changes the permission with chmod -R" $ do
      result <- testCric $ chmodRec 0o777 "filepath"
      result `shouldSatisfy` (`outputContains` "chmod -R 777 filepath")

  describe "chown" $ do
    it "changes the owner with chown" $ do
      result <- testCric $ chown "user" (Just "group") "filepath"
      result `shouldSatisfy` (`outputContains` "chown user:group filepath")

    it "does not specify the group if Nothing" $ do
      result <- testCric $ chown "user" Nothing "filepath"
      result `shouldSatisfy` (`outputContains` "chown user filepath")

  describe "chownRec" $ do
    it "changes the owner with chown -R" $ do
      result <- testCric $ chownRec "user" (Just "group") "filepath"
      result `shouldSatisfy` (`outputContains` "chown -R user:group filepath")

    it "does not specify the group if Nothing" $ do
      result <- testCric $ chownRec "user" Nothing "filepath"
      result `shouldSatisfy` (`outputContains` "chown -R user filepath")

  describe "symlink" $ do
    it "(re)creates a symbolic link" $ do
      result <- testCric $ symlink "from" "to"
      result `shouldSatisfy` (`outputContains` "ln -sf to from")

  do
    let mock = mockCommand "ls /some/file" (0, "/some/file\n")
             $ mockCommand "ls /some/dir"  (0, "file1\tfile2\n")
             $ mockCommand "ls /not/there" (2, "/not/there\n")
             $ defaultSshMock
        testWithMock cricAction = testInstallWith mock cricAction nullLogger defaultContext defaultServer

    describe "doesFileExist" $ do
      it "returns True if the file exists" $ do
        result <- testWithMock $ doesFileExist "/some/file"
        result `shouldBe` True

      it "returns False if it is a directory" $ do
        result <- testWithMock $ doesFileExist "/some/dir"
        result `shouldBe` False

      it "returns false if it does not exist" $ do
        result <- testWithMock $ doesFileExist "/not/there"
        result `shouldBe` False

    describe "doesDirectoryExist" $ do
      it "returns True if the directory exists" $ do
        result <- testWithMock $ doesDirectoryExist "/some/dir"
        result `shouldBe` True

      it "returns False if it is a file" $ do
        result <- testWithMock $ doesDirectoryExist "/some/file"
        result `shouldBe` False

      it "returns false if it does not exist" $ do
        result <- testWithMock $ doesDirectoryExist "/not/there"
        result `shouldBe` False

outputContains :: Result -> String -> Bool
outputContains res str = isInfixOf str . BS.unpack $ outFromResult res
