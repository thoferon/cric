module Cric.FileSystem
  ( createFile, removeFile
  , copyFile, moveFile
  , chmod, chown
  ) where

import Numeric (showOct)

import Cric

createFile :: MonadCric m => FilePath -> m Result
createFile = exec . ("touch " ++)

removeFile :: MonadCric m => FilePath -> m Result
removeFile = exec . ("rm -rf " ++)

copyFile :: MonadCric m => FilePath -> FilePath -> m Result
copyFile from to = exec $ "cp -rf " ++ from ++ " " ++ to

moveFile :: MonadCric m => FilePath -> FilePath -> m Result
moveFile from to = exec $ "mv -rf " ++ from ++ " " ++ to

chmod :: MonadCric m => Int -> FilePath -> m Result
chmod perm path = exec $ "chmod " ++ showOct perm " " ++ path

chown :: MonadCric m => String -> String -> FilePath -> m Result
chown username group path = exec $ "chown " ++ username ++ ":" ++ group ++ " " ++ path
