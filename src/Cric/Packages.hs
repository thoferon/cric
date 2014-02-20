{-# LANGUAGE FlexibleInstances #-}

module Cric.Packages
  ( PackageManager(..), PkgManagerError(..), Package(..)
  , getPackageManager
  , installPackage
  , installWithRPM, installWithYum, installWithAPT
  , removePackage
  , removeWithRPM, removeWithYum, removeWithAPT
  ) where

import qualified Data.ByteString.Char8 as BS

import           Cric
import           Cric.SystemInfo

class Show p => Package p where
  urlForRPM  :: p -> String
  urlForRPM _ = ""

  nameForYum :: p -> String
  nameForYum _ = ""

  nameForAPT :: p -> String
  nameForAPT _ = ""

instance Package String where
  urlForRPM  = id
  nameForYum = id
  nameForAPT = id

data PackageManager = RPM | Yum | APT
                    | UnknownPackageManager
                    deriving (Show, Eq)

data PkgManagerError = NoPackageManagerFound
                     | UnknownPkgManagerError Result
                     -- ^ Unknown error together with status code and output
                     deriving (Show, Eq)

-- | Find a package manager on the server by testing if the command is available.
getPackageManager :: MonadCric m => m PackageManager
getPackageManager = getPackageManager' [ ("yum", Yum)
                                       , ("apt-get", APT)
                                       , ("rpm", RPM)
                                       ]
  where
    getPackageManager' :: MonadCric m => [(String, PackageManager)] -> m PackageManager
    getPackageManager' [] = return UnknownPackageManager
    getPackageManager' ((cmd,mgr):rest) = do
      test <- testCommand cmd
      if test
        then return mgr
        else getPackageManager' rest

-- | Install a package with the package manager found.
installPackage :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
installPackage pkg = do
  logMsg Info $ "Installing " ++ show pkg ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> installWithYum pkg
    APT -> installWithAPT pkg
    RPM -> installWithRPM pkg
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error installing " ++ show pkg ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ show pkg ++ " installed successfully."
  return result

installWithRPM :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
installWithRPM = execManager . ("rpm -i "++) . urlForRPM

installWithYum :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
installWithYum = execManager . ("yum install -y "++) . nameForYum

installWithAPT :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
installWithAPT = execManager . ("apt-get install -y "++) . nameForAPT

-- | Remove a package with the package manager found.
removePackage :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
removePackage pkg = do
  logMsg Info $ "Removing " ++ show pkg ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> removeWithYum pkg
    APT -> removeWithAPT pkg
    RPM -> removeWithRPM pkg
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error removing " ++ show pkg ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ show pkg ++ " removed successfully."
  return result

removeWithRPM :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
removeWithRPM = execManager . ("rpm -e "++) . urlForRPM

removeWithYum :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
removeWithYum = execManager . ("yum remove -y "++) . nameForYum

removeWithAPT :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
removeWithAPT = execManager . ("apt-get remove -y "++) . nameForAPT

execManager :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
execManager cmd = do
  result <- exec cmd
  return $ if isSuccess result
             then Right $ outFromResult result
             else Left $ UnknownPkgManagerError result
