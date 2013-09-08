module Cric.FileSystemSpec (
  test
  ) where

import Test.Hspec
import SpecHelpers

import Cric
import Cric.FileSystem

import Data.List
import qualified Data.ByteString.Char8 as BS

test :: Spec
test = do
  describe "createFile" $ do
    it "executes touch" $ do
      result <- testCric $ createFile "filepath"
      result `shouldSatisfy` (`outputContains` "touch filepath")

  describe "removeFile" $ do
    it "executes rm -rf" $ do
      result <- testCric $ removeFile "filepath"
      result `shouldSatisfy` (`outputContains` "rm -rf filepath")

  describe "copyFile" $ do
    it "executes cp -rf" $ do
      result <- testCric $ copyFile "from" "to"
      result `shouldSatisfy` (`outputContains` "cp -rf from to")

  describe "moveFile" $ do
    it "executes mv -rf" $ do
      result <- testCric $ moveFile "from" "to"
      result `shouldSatisfy` (`outputContains` "mv -rf from to")

  describe "chmod" $ do
    it "changes the permission with chmod" $ do
      result <- testCric $ chmod 0o777 "filepath"
      result `shouldSatisfy` (`outputContains` "chmod 777 filepath")

  describe "chown" $ do
    it "changes the owner with chown" $ do
      result <- testCric $ chown "user" "group" "filepath"
      result `shouldSatisfy` (`outputContains` "chown user:group filepath")

outputContains :: Result -> String -> Bool
outputContains res str = isInfixOf str . BS.unpack $ outputFromResult res
