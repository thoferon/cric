module Cric (
  -- Types
  Server(..), AuthenticationType(..)
  , Context(..)
  , Cric
  , CricT(..)
  , Result(..)
  , Logger, LogLevel(..)
  , FileTransferOptions(..)
  , Executor
  -- Factories
  , defaultServer, autoServer
  , defaultContext
  , stdoutLogger, debugLogger
  , defaultFileTransferOptions, dfto
  -- Functions
  , runCric
  , install, installOn
  , exec, run
  , isSuccess, outputFromResult
  , sendFile
  , getServer
  , getContext
  , log
  , testCommand
  , withChangedContext
  , asUser
  , inDir
  , withEnv
  ) where

import Prelude hiding (log)

import Cric.Core
import Cric.TypeDefs
