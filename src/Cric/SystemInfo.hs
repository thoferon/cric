{-# LANGUAGE OverloadedStrings #-}

module Cric.SystemInfo (
  OperatingSystem(..)
  , getOS
  ) where

import Cric

import Data.Word8 (Word8)
import Data.ByteString.Lazy.Char8 () -- For OverloadedStrings

import qualified Data.ByteString.Lazy as BL

data OperatingSystem = Linux
                     | FreeBSD
                     | UnknownOS BL.ByteString
                     deriving (Show, Eq)

getOS :: Cric OperatingSystem
getOS = do
  result <- run "uname -o"
  let name = firstLine result
  return $ case name of
    "GNU/Linux" -> Linux
    "FreeBSD" -> FreeBSD
    other -> UnknownOS other

firstLine :: BL.ByteString -> BL.ByteString
firstLine = BL.takeWhile isNotNewLine
  where isNotNewLine :: Word8 -> Bool
        isNotNewLine = (/=10)
