module Cric
  ( Server(..), AuthenticationType(..)
  , Context(..)
  , Cric
  , CricT(..)
  , Result(..)
  , Logger, LogLevel(..)
  , FileTransferOptions(..)
  , Executor
  , MonadCric(..)
  -- Factories
  , defaultServer, autoServer
  , defaultContext
  , stdoutLogger, debugLogger
  , defaultFileTransferOptions, dfto
  -- Functions
  , runCric
  , install, installOn
  , run
  , isSuccess, outputFromResult
--  , testCommand
  , asUser
  , inDir
  , withEnv
  ) where

import Prelude hiding (log)

import Cric.Core
import Cric.TypeDefs
import Cric.MonadCric
