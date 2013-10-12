{-# LANGUAGE OverloadedStrings #-}

module Cric.SystemInfo
  ( OperatingSystem(..)
  , getOS
  , testCommand
  ) where

import qualified Data.ByteString.Char8 as BS

import           Cric

data OperatingSystem
  = Linux
  | FreeBSD
  | UnknownOS BS.ByteString
  deriving (Show, Eq)

-- | Find the operating system of the server.
getOS :: MonadCric m => m OperatingSystem
getOS = do
  result <- run "uname -o"
  let name = firstLine result
  return $ case name of
    "GNU/Linux" -> Linux
    "FreeBSD"   -> FreeBSD
    other       -> UnknownOS other

firstLine :: BS.ByteString -> BS.ByteString
firstLine = BS.takeWhile (/='\n')

-- | Test if a command is available on the server.
testCommand :: MonadCric m => String -> m Bool
testCommand cmd = do
  res <- exec $ "which " ++ cmd
  return $ case res of
    Success _   -> True
    Failure _ _ -> False
