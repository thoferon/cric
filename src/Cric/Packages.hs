{-# LANGUAGE FlexibleInstances #-}

module Cric.Packages
  ( PackageManager(..), PkgManagerError(..), Package(..)
  , getPackageManager
  , installPackage
  , installWithRPM, installWithYum, installWithAPT, installWithPkgAdd
  , removePackage
  , removeWithRPM, removeWithYum, removeWithAPT, removeWithPkgAdd
  ) where

import qualified Data.ByteString.Char8 as BS

import           Cric
import           Cric.SystemInfo

class Show p => Package p where
  urlForRPM  :: OperatingSystem -> p -> String
  urlForRPM _ _ = ""

  nameForYum :: OperatingSystem -> p -> String
  nameForYum _ _ = ""

  nameForAPT :: OperatingSystem -> p -> String
  nameForAPT _ _ = ""

  nameForPkgAdd :: OperatingSystem -> p -> String
  nameForPkgAdd _ _ = ""

instance Package String where
  urlForRPM     = flip const
  nameForYum    = flip const
  nameForAPT    = flip const
  nameForPkgAdd = flip const

data PackageManager = RPM | Yum | APT | PkgAdd
                    | UnknownPackageManager
                    deriving (Show, Eq)

data PkgManagerError = NoPackageManagerFound
                     | UnknownPkgManagerError Result
                     -- ^ Unknown error together with status code and output
                     deriving (Show, Eq)

-- | Find a package manager on the server by testing if the command is available.
getPackageManager :: MonadCric m => m PackageManager
getPackageManager = getPackageManager'
    [ ("yum", Yum)
    , ("apt-get", APT)
    , ("rpm", RPM)
    , ("pkg_add", PkgAdd)
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
  os     <- getOS
  result <- case pkgMgr of
    Yum    -> installWithYum    os pkg
    APT    -> installWithAPT    os pkg
    RPM    -> installWithRPM    os pkg
    PkgAdd -> installWithPkgAdd os pkg
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error installing " ++ show pkg ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ show pkg ++ " installed successfully."
  return result

installWithRPM :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
installWithRPM os pkg = execManager . ("rpm -i "++) $ urlForRPM os pkg

installWithYum :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
installWithYum os pkg = execManager . ("yum install -y "++) $ nameForYum os pkg

installWithAPT :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
installWithAPT os pkg = execManager . ("apt-get install -y "++) $ nameForAPT os pkg

installWithPkgAdd :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
installWithPkgAdd os pkg = execManager . ("pkg_add "++) $ nameForPkgAdd os pkg

-- | Remove a package with the package manager found.
removePackage :: (MonadCric m, Package p) => p -> m (Either PkgManagerError BS.ByteString)
removePackage pkg = do
  logMsg Info $ "Removing " ++ show pkg ++ " ..."
  pkgMgr <- getPackageManager
  os <- getOS
  result <- case pkgMgr of
    Yum    -> removeWithYum    os pkg
    APT    -> removeWithAPT    os pkg
    RPM    -> removeWithRPM    os pkg
    PkgAdd -> removeWithPkgAdd os pkg
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error removing " ++ show pkg ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ show pkg ++ " removed successfully."
  return result

removeWithRPM :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
removeWithRPM os pkg = execManager . ("rpm -e "++) $ urlForRPM os pkg

removeWithYum :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
removeWithYum os pkg = execManager . ("yum remove -y "++) $ nameForYum os pkg

removeWithAPT :: (MonadCric m, Package p) => OperatingSystem -> p -> m (Either PkgManagerError BS.ByteString)
removeWithAPT os pkg = execManager . ("apt-get remove -y "++) $ nameForAPT os pkg

removeWithPkgAdd :: (MonadCric m, Package p) => OperatingSystem ->  p -> m (Either PkgManagerError BS.ByteString)
removeWithPkgAdd os pkg = execManager . ("pkg_delete "++) $ nameForPkgAdd os pkg

execManager :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
execManager cmd = do
  result <- exec cmd
  return $ if isSuccess result
             then Right $ outFromResult result
             else Left $ UnknownPkgManagerError result
