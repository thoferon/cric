module Cric.MonadCric
  ( MonadCric(..)
  , run
  , asUser
  , inDir
  , withEnv
  ) where

import           Control.Monad

import           Data.List
import qualified Data.ByteString.Char8 as BS

import           Cric.TypeDefs

-- | Typeclass to make a stack of monad transformers easy to use without having to lift.
class Monad m => MonadCric m where
  -- | Execute a command on the remote server.
  exec :: String -- ^ Command
       -> m Result

  -- | Transfer a file to the remote server.
  --
  -- If a md5 hash has been specified, the function returns whether the hash matches.
  -- Otherwise, it returns the number of bytes transferred.
  sendFile :: FilePath                -- ^ Local path
           -> FilePath                -- ^ Remote path
           -> FileTransferOptions     -- ^ Extra options
           -> m (Either Integer Bool) -- ^ Bytes transferred or md5 check success

  -- | Execute a cric action with a modified context.
  withChangedContext :: (Context -> Context) -- ^ Function to change the context
                     -> m a                  -- ^ Cric action
                     -> m a

  -- | Log a message with the logger specified.
  logMsg :: LogLevel -- ^ Log level (e.g. 'Warning')
         -> String   -- ^ Message
         -> m ()

  -- | Return the current context of execution.
  getContext :: m Context

  -- | Return the connection information of the current server.
  getServer          :: m Server

-- | Helper around 'exec' which simply returns the output without information about the status code.
run :: MonadCric m => String -> m BS.ByteString
run cmd = outFromResult `liftM` exec cmd

-- | Execute a Cric action as a different user.
asUser :: MonadCric m
       => String -- ^ Username
       -> m a    -- ^ Cric action
       -> m a
asUser u cric = do
    testResult <- asUser' $ exec "echo test if we can log in"
    unless (isSuccess testResult) $
           logMsg Warning $ "Can't execute a command as user " ++ u
    asUser' cric
  where
    asUser' = withChangedContext $ \context -> context { currentUser = u }

-- | Execute a Cric action in a different working directory.
inDir :: MonadCric m => FilePath -> m a -> m a
inDir d cric = do
    testResult <- inDir' $ exec "ls"
    unless (isSuccess testResult) $
           logMsg Warning $ "Can't change directory to " ++ d
    inDir' cric
  where
    inDir' = withChangedContext $ \context -> context { currentDir = d }

-- | Execute a Cric action with different environment variables.
withEnv :: MonadCric m
        => [(String, String)] -- ^ Pairs name/value (e.g. [("PATH","/bin:/usr/bin")]
        -> m a                -- ^ Cric action
        -> m a
withEnv e = withChangedContext $ \context -> context { currentEnv = mergeEnv (currentEnv context) e }
  where
    mergeEnv old [] = old
    mergeEnv old (ev@(n,_):evs) = let (_,cleanOld) = partition ((==n) . fst) old
                                  in mergeEnv (ev : cleanOld) evs
