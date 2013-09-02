module Cric.FileSystem (
  createFile, removeFile
  , copyFile, moveFile
  , chmod, chown
  ) where

import Cric

import Numeric (showOct)

createFile :: Monad m => FilePath -> CricT m Result
createFile = exec . ("touch " ++)

removeFile :: Monad m => FilePath -> CricT m Result
removeFile = exec . ("rm -rf " ++)

copyFile :: Monad m => FilePath -> FilePath -> CricT m Result
copyFile from to = exec $ "cp -rf " ++ from ++ " " ++ to

moveFile :: Monad m => FilePath -> FilePath -> CricT m Result
moveFile from to = exec $ "mv -rf " ++ from ++ " " ++ to

chmod :: Monad m => Int -> FilePath -> CricT m Result
chmod perm path = exec $ "chmod " ++ showOct perm " " ++ path

chown :: Monad m => String -> String -> FilePath -> CricT m Result
chown username group path = exec $ "chown " ++ username ++ ":" ++ group ++ " " ++ path
