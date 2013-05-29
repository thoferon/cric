module Cric (
  -- Types
  Server(..), AuthenticationType(..)
  , Context(..)
  , Cric
  , Result(..)
  , Logger, LogLevel(..)
  , FileTransferOptions(..)
  -- Factories
  , defaultServer, autoServer
  , defaultContext
  , stdoutLogger, debugLogger
  , defaultFileTransferOptions, dfto
  -- Functions
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
