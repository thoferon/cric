{-# LANGUAGE OverloadedStrings #-}

module Cric.SystemInfo (
  OperatingSystem(..)
  , getOS
  ) where

import Cric

import qualified Data.ByteString.Char8 as BS

data OperatingSystem = Linux
                     | FreeBSD
                     | UnknownOS BS.ByteString
                     deriving (Show, Eq)

getOS :: Monad m => CricT m OperatingSystem
getOS = do
  result <- run "uname -o"
  let name = firstLine result
  return $ case name of
    "GNU/Linux" -> Linux
    "FreeBSD" -> FreeBSD
    other -> UnknownOS other

firstLine :: BS.ByteString -> BS.ByteString
firstLine = BS.takeWhile (/='\n')
