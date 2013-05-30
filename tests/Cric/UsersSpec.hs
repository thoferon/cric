{-# LANGUAGE OverloadedStrings #-}

module Cric.UsersSpec (
  test
  ) where

import Test.Hspec
import SpecHelpers

import Data.List

import Cric
import Cric.Users

test :: Spec
test = do
  describe "createUser" $ do
    it "builds the command with different parameters" $ do
      let opts = defaultUserOptions {
        loginGroup = Just "logingroup"
        , groups   = ["group1", "group2"]
        , uid      = Just 1337
        , shell    = Just "/bin/sh"
      }
      let cric = createUser "testusername" opts

      let isCmdGood cmd = all (`isInfixOf` cmd)
                            ["adduser"
                            , "testusername"
                            , "-s /bin/sh"
                            , "-u 1337"
                            , "-G group1,group2"
                            , "-g logingroup"]

      let mockFunc cmds = if any isCmdGood cmds
                          then Just $ return (0, ["good"])
                          else Nothing
      let sshMock = SshMock [mockFunc] []

      result <- testCricWith sshMock cric
      result `shouldBe` Success "good"

    it "doesn't specify anything if the options are set to Nothing/[]" $ do
      let opts = defaultUserOptions {
        loginGroup = Nothing
        , groups   = []
        , uid      = Nothing
        , shell    = Nothing
      }
      let cric = createUser "testusername" opts

      let isCmdGood cmd = not $ any (`isInfixOf` cmd) [ "-s" , "-u" , "-G" , "-g"]
      let mockFunc cmds = if any ("adduser" `isInfixOf`) cmds
                          then if all isCmdGood cmds
                               then Just $ return (0, ["good"])
                               else Just $ return (1, ["bad"])
                          else Nothing
      let sshMock = SshMock [mockFunc] []

      result <- testCricWith sshMock cric
      result `shouldBe` Success "good"

  describe "removeUser" $ do
    it "detects and uses rmuser" $ do
      let mock = mockCommand "rmuser" (0, ["rmuser called"])
               . mockCommand "which rmuser" (0, ["/bin/rmuser"])
               $ defaultSshMock
      result <- testCricWith mock $ removeUser "username"
      result `shouldBe` Success "rmuser called"

    it "uses deluser if rmuser is not found" $ do
      let mock = mockCommand "deluser" (0, ["deluser called"]) defaultSshMock
      result <- testCricWith mock $ removeUser "username"
      result `shouldBe` Success "deluser called"
