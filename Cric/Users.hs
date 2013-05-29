module Cric.Users (
  UserOptions(..)
  , defaultUserOptions, duo
  , createUser, removeUser
  ) where

import Prelude hiding (log)

import Data.List (foldl')

import Cric

data UserOptions = UserOptions {
  loginGroup :: Maybe String
  , groups     :: [String]
  , uid        :: Maybe Int
  , shell      :: Maybe String
} deriving Show

defaultUserOptions :: UserOptions
defaultUserOptions = UserOptions {
  loginGroup = Nothing
  , groups     = []
  , uid        = Nothing
  , shell      = Nothing
}

duo :: UserOptions
duo = defaultUserOptions

createUser :: String -> UserOptions -> Cric Result
createUser u opts = do
    log LInfo $ "Creating user " ++ u ++ " ..."
    let cmdWithOptions = addLoginGroup (loginGroup opts)
                         . addGroups (groups opts)
                         . addUID (uid opts)
                         . addShell (shell opts)
                         $ "adduser"
    exec $ cmdWithOptions ++ " " ++ u
  where
    addShell Nothing        = id
    addShell (Just s)       = (++ " -s " ++ s)
    addUID Nothing          = id
    addUID (Just i)         = (++ " -u " ++ show i)
    addGroups []            = id
    addGroups gs            = let gsStr = foldl' (\acc g -> acc++","++g) (head gs) (tail gs)
                              in (++ " -G " ++ gsStr)
    addLoginGroup Nothing   = id
    addLoginGroup (Just lg) = (++ " -g " ++ lg)

removeUser :: String -> Cric Result
removeUser u = do
  log LInfo $ "Removing user " ++ u ++ " ..."
  test <- testCommand "rmuser"
  let cmd = if test then "rmuser " else "deluser "
  exec $ cmd ++ u
