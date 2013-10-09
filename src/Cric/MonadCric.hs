module Cric.MonadCric
  ( MonadCric(..)
  ) where

import Cric.TypeDefs

class Monad m => MonadCric m where
  exec               :: String -> m Result
  sendFile           :: FilePath -> FilePath -> FileTransferOptions -> m (Either Integer Bool)
  withChangedContext :: (Context -> Context) -> m a -> m a
  logMsg             :: LogLevel -> String -> m ()
  getContext         :: m Context
  getServer          :: m Server
