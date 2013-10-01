module Cric.Users
  ( UserOptions(..)
  , defaultUserOptions, duo
  , createUser, removeUser
  ) where

import Data.Default
import Data.List (foldl')

import Cric
import Cric.SystemInfo

data UserOptions = UserOptions
  { loginGroup :: Maybe String
  , groups     :: [String]
  , uid        :: Maybe Int
  , shell      :: Maybe String
  } deriving Show

defaultUserOptions :: UserOptions
defaultUserOptions = UserOptions
  { loginGroup = Nothing
  , groups     = []
  , uid        = Nothing
  , shell      = Nothing
  }

duo :: UserOptions
duo = defaultUserOptions

instance Default UserOptions where
  def = duo

createUser :: MonadCric m => String -> UserOptions -> m Result
createUser u opts = do
    logMsg LInfo $ "Creating user " ++ u ++ " ..."
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

removeUser :: MonadCric m => String -> m Result
removeUser u = do
  logMsg LInfo $ "Removing user " ++ u ++ " ..."
  test <- testCommand "rmuser"
  let cmd = if test then "rmuser " else "deluser "
  exec $ cmd ++ u
