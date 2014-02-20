module Cric.FileSystem
  ( createFile, removeFile, createDirectory, removeDirectory
  , copyFile, moveFile, copyDirectory, moveDirectory
  , chmod, chown, chmodRec, chownRec
  , symlink
  , doesFileExist, doesDirectoryExist
  ) where

import qualified Data.ByteString.Char8 as BS

import           Numeric (showOct)

import           Cric

-- | Create a file on the server.
createFile :: MonadCric m
           => FilePath -- ^ Remote path
           -> m Result
createFile = exec . ("touch " ++)

-- | Create a directory on the server as well as its parents.
createDirectory :: MonadCric m
                => FilePath -- ^ Remote path
                -> m Result
createDirectory = exec . ("mkdir -p " ++)

-- | Remove a file from the server.
removeFile :: MonadCric m
           => FilePath -- ^ Remote path
           -> m Result
removeFile = exec . ("rm -f " ++)

-- | Remove a directory with its contents.
removeDirectory :: MonadCric m
                => FilePath
                -> m Result
removeDirectory = exec . ("rm -rf " ++)

-- | Copy a file from two remote paths.
copyFile :: MonadCric m
         => FilePath -- ^ Source path
         -> FilePath -- ^ Target path
         -> m Result
copyFile from to = exec $ "cp -f " ++ from ++ " " ++ to

-- | Copy a directory recursively from two remote paths.
copyDirectory :: MonadCric m
              => FilePath -- ^ Source path
              -> FilePath -- ^ Target path
              -> m Result
copyDirectory from to = exec $ "cp -rf " ++ from ++ " " ++ to

-- | Move a file from two remote paths.
moveFile :: MonadCric m
         => FilePath -- ^ Source path
         -> FilePath -- ^ Target path
         -> m Result
moveFile from to = exec $ "mv -f " ++ from ++ " " ++ to

-- | Move a directory recursively from two remote paths.
moveDirectory :: MonadCric m
              => FilePath -- ^ Source path
              -> FilePath -- ^ Target path
              -> m Result
moveDirectory from to = exec $ "mv -rf " ++ from ++ " " ++ to

-- | Change the permissions of a file on the server.
chmod :: MonadCric m
      => Int      -- ^ File mode (e.g. 0o777)
      -> FilePath -- ^ Remote path
      -> m Result
chmod perm path = exec $ "chmod " ++ showOct perm " " ++ path

-- | Change the permissions recursively of a directory on the server.
chmodRec :: MonadCric m
         => Int      -- ^ File mode (e.g. 0o777)
         -> FilePath -- ^ Remote path
         -> m Result
chmodRec perm path = exec $ "chmod -R " ++ showOct perm " " ++ path

-- | Change the owner (user and group) of a file on the server.
chown :: MonadCric m
      => String       -- ^ Username
      -> Maybe String -- ^ Group
      -> FilePath     -- ^ Remote path
      -> m Result
chown username mgroup path =
  let groupFragment = maybe "" (":" ++) mgroup
  in exec $ "chown " ++ username ++ groupFragment ++ " " ++ path

-- | Change the owner (user and group) of a file on the server.
chownRec :: MonadCric m
         => String       -- ^ Username
         -> Maybe String -- ^ Group
         -> FilePath     -- ^ Remote path
         -> m Result
chownRec username mgroup path =
  let groupFragment = maybe "" (":" ++) mgroup
  in exec $ "chown -R " ++ username ++ groupFragment ++ " " ++ path

-- | Create a symbolic link on the remote server. It overwrites it if already present.
symlink :: MonadCric m
        => FilePath -- ^ Source Path
        -> FilePath -- ^ Target Path
        -> m Result
symlink from to = exec $ "ln -sf " ++ to ++ " " ++ from

-- | Check if the file exists and is not a directory.
--
-- It will follow symbolic links.
doesFileExist :: MonadCric m
              => FilePath
              -> m Bool
doesFileExist path = do
  result <- exec $ "ls " ++ path
  let checkExistence    = isSuccess result
      checkNotDirectory = BS.pack path `BS.isPrefixOf` outFromResult result
  return $ checkExistence && checkNotDirectory

-- | Check if the directory exists.
--
-- It will follow symbolic links.
doesDirectoryExist :: MonadCric m
                   => FilePath
                   -> m Bool
doesDirectoryExist path = do
  result <- exec $ "ls " ++ path
  let checkExistence    = isSuccess result
      checkNotDirectory = BS.pack path `BS.isPrefixOf` outFromResult result
  return $ checkExistence && not checkNotDirectory
