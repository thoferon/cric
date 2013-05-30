module Cric.FileSystem (
  createFile, removeFile
  , copyFile, moveFile
  , chmod, chown
  ) where

import Cric

import Numeric (showOct)

createFile :: FilePath -> Cric Result
createFile = exec . ("touch " ++)

removeFile :: FilePath -> Cric Result
removeFile = exec . ("rm -rf " ++)

copyFile :: FilePath -> FilePath -> Cric Result
copyFile from to = exec $ "cp -rf " ++ from ++ " " ++ to

moveFile :: FilePath -> FilePath -> Cric Result
moveFile from to = exec $ "mv -rf " ++ from ++ " " ++ to

chmod :: Int -> FilePath -> Cric Result
chmod perm path = exec $ "chmod " ++ showOct perm " " ++ path

chown :: String -> String -> FilePath -> Cric Result
chown username group path = exec $ "chown " ++ username ++ ":" ++ group ++ " " ++ path
