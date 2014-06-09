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
  , stdoutLogger, debugLogger, traceLogger
  , defaultFileTransferOptions, dfto
  -- Functions
  , runCric
  , install, installOn
  , mkExecutor
  , run
  , isSuccess
  , outFromResult, errFromResult
--  , testCommand
  , asUser
  , inDir
  , withEnv
  ) where

import Prelude hiding (log)

import Cric.Core
import Cric.TypeDefs
import Cric.MonadCric
