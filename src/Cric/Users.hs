module Cric.Users
  ( UserOptions(..)
  , defaultUserOptions, duo
  , createUser, removeUser
  ) where

import Data.Default
import Data.List (foldl')

import Cric
import Cric.SystemInfo

-- | Extra options for a user, see 'createUser'.
data UserOptions = UserOptions
  { loginGroup :: Maybe String -- ^ The principal group of the user
  , groups     :: [String]     -- ^ List of additional groups
  , uid        :: Maybe Int    -- ^ User ID
  , shell      :: Maybe String
  } deriving Show

-- | Default extra user options.
defaultUserOptions :: UserOptions
defaultUserOptions = UserOptions
  { loginGroup = Nothing
  , groups     = []
  , uid        = Nothing
  , shell      = Nothing
  }

-- | Alias for 'defaultUserOptions'.
duo :: UserOptions
duo = defaultUserOptions

instance Default UserOptions where def = duo

-- | Create a user on the server.
createUser :: MonadCric m
              => String      -- ^ Username
              -> UserOptions -- ^ Extra options
              -> m Result
createUser u opts = do
    logMsg Info $ "Creating user " ++ u ++ " ..."
    os <- getOS
    let cmd = case os of
                OpenBSD -> "useradd" -- adduser would go interactive without options
                _       -> "adduser"
    let cmdWithOptions = addLoginGroup (loginGroup opts)
                       . addGroups (groups opts)
                       . addUID (uid opts)
                       . addShell (shell opts)
                       $ cmd ++ " -m"
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

-- | Remove a user from the server.
removeUser :: MonadCric m
           => String -- ^ Username
           -> m Result
removeUser u = do
  logMsg Info $ "Removing user " ++ u ++ " ..."
  test <- testCommand "rmuser"
  let cmd = if test then "rmuser " else "deluser "
  exec $ cmd ++ u
