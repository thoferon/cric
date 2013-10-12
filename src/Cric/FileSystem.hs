module Cric.FileSystem
  ( createFile, removeFile
  , copyFile, moveFile
  , chmod, chown
  ) where

import Numeric (showOct)

import Cric

-- | Create a file on the server.
createFile :: MonadCric m
           => FilePath -- ^ Remote path
           -> m Result
createFile = exec . ("touch " ++)

-- | Remove a file from the server.
removeFile :: MonadCric m
           => FilePath -- ^ Remote path
           -> m Result
removeFile = exec . ("rm -rf " ++)

-- | Copy a file from two remote paths.
copyFile :: MonadCric m
         => FilePath -- ^ Source path
         -> FilePath -- ^ Target path
         -> m Result
copyFile from to = exec $ "cp -rf " ++ from ++ " " ++ to

-- | Move a file from two remote paths.
moveFile :: MonadCric m
         => FilePath -- ^ Source path
         -> FilePath -- ^ Target path
         -> m Result
moveFile from to = exec $ "mv -rf " ++ from ++ " " ++ to

-- | Change the permissions of a file on the server.
chmod :: MonadCric m
      => Int      -- ^ File mode (e.g. 0o777)
      -> FilePath -- ^ Remote path
      -> m Result
chmod perm path = exec $ "chmod " ++ showOct perm " " ++ path

-- | Change the owner (user and group) of a file on the server.
chown :: MonadCric m
      => String   -- ^ Username
      -> String   -- ^ Group
      -> FilePath -- ^ Remote path
      -> m Result
chown username group path = exec $ "chown " ++ username ++ ":" ++ group ++ " " ++ path
