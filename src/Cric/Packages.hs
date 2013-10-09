module Cric.Packages
  ( PackageManager(..), PkgManagerError(..)
  , getPackageManager
  , installPackage
  , installWithRPM, installWithYum, installWithAPT
  , removePackage
  , removeWithRPM, removeWithYum, removeWithAPT
  ) where

import qualified Data.ByteString.Char8 as BS

import           Cric
import           Cric.SystemInfo

data PackageManager = RPM | Yum | APT
                    | UnknownPackageManager
                    deriving (Show, Eq)

data PkgManagerError = NoPackageManagerFound
                     | UnknownPkgManagerError Int BS.ByteString
                     deriving (Show, Eq)

getPackageManager :: MonadCric m => m PackageManager
getPackageManager = getPackageManager' [("yum", Yum), ("apt-get", APT), ("rpm", RPM)]
  where
    getPackageManager' :: MonadCric m => [(String, PackageManager)] -> m PackageManager
    getPackageManager' [] = return UnknownPackageManager
    getPackageManager' ((cmd,mgr):rest) = do
      test <- testCommand cmd
      case test of
        True -> return mgr
        False -> getPackageManager' rest

installPackage :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
installPackage pkgName = do
  logMsg Info $ "Installing " ++ pkgName ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> installWithYum pkgName
    APT -> installWithAPT pkgName
    RPM -> installWithRPM pkgName
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error installing " ++ pkgName ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ pkgName ++ " installed successfully."
  return result

installWithRPM :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
installWithRPM = execManager . ("rpm -i "++)

installWithYum :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
installWithYum = execManager . ("yum install -y "++)

installWithAPT :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
installWithAPT = execManager . ("apt-get install -y "++)

removePackage :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
removePackage pkgName = do
  logMsg Info $ "Removing " ++ pkgName ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> removeWithYum pkgName
    APT -> removeWithAPT pkgName
    RPM -> removeWithRPM pkgName
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> logMsg Error $ "Error removing " ++ pkgName ++ " (" ++ show err ++ ")"
    Right _  -> logMsg Info  $ "Package " ++ pkgName ++ " removed successfully."
  return result

removeWithRPM :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
removeWithRPM = execManager . ("rpm -e "++)

removeWithYum :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
removeWithYum = execManager . ("yum remove -y "++)

removeWithAPT :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
removeWithAPT = execManager . ("apt-get remove -y "++)

execManager :: MonadCric m => String -> m (Either PkgManagerError BS.ByteString)
execManager cmd = do
  result <- exec cmd
  return $ case result of
    Success output      -> Right output
    Failure code output -> Left $ UnknownPkgManagerError code output
