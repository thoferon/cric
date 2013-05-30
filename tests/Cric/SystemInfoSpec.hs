{-# LANGUAGE OverloadedStrings #-}

module Cric.SystemInfoSpec (
  test
  ) where

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
    it "detects Linux"   $ testGetOS (0, ["GNU/Linux"]) Linux
    it "detects FreeBSD" $ testGetOS (0, ["FreeBSD"])   FreeBSD

    it "returns Unknown with the value if not found" $ do
      testGetOS (0, ["unrecognized"]) $ UnknownOS "unrecognized"
