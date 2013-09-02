module Cric.Packages (
  PackageManager(..), PkgManagerError(..)
  , getPackageManager
  , installPackage
  , installWithRPM, installWithYum, installWithAPT
  , removePackage
  , removeWithRPM, removeWithYum, removeWithAPT
  ) where

import Prelude hiding (log)

import Cric

import qualified Data.ByteString.Lazy as BL

data PackageManager = RPM | Yum | APT
                    | UnknownPackageManager
                    deriving (Show, Eq)

data PkgManagerError = NoPackageManagerFound
                     | UnknownPkgManagerError Int BL.ByteString
                     deriving (Show, Eq)

getPackageManager :: Monad m => CricT m PackageManager
getPackageManager = getPackageManager' [("yum", Yum), ("apt-get", APT), ("rpm", RPM)]
  where
    getPackageManager' :: Monad m => [(String, PackageManager)] -> CricT m PackageManager
    getPackageManager' [] = return UnknownPackageManager
    getPackageManager' ((cmd,mgr):rest) = do
      test <- testCommand cmd
      case test of
        True -> return mgr
        False -> getPackageManager' rest

installPackage :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
installPackage pkgName = do
  log LInfo $ "Installing " ++ pkgName ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> installWithYum pkgName
    APT -> installWithAPT pkgName
    RPM -> installWithRPM pkgName
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> log LError $ "Error installing " ++ pkgName ++ " (" ++ show err ++ ")"
    Right _  -> log LInfo  $ "Package " ++ pkgName ++ " installed successfully."
  return result

installWithRPM :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
installWithRPM = execManager . ("rpm -i "++)

installWithYum :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
installWithYum = execManager . ("yum install -y "++)

installWithAPT :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
installWithAPT = execManager . ("apt-get install -y "++)

removePackage :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
removePackage pkgName = do
  log LInfo $ "Removing " ++ pkgName ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    Yum -> removeWithYum pkgName
    APT -> removeWithAPT pkgName
    RPM -> removeWithRPM pkgName
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> log LError $ "Error removing " ++ pkgName ++ " (" ++ show err ++ ")"
    Right _  -> log LInfo  $ "Package " ++ pkgName ++ " removed successfully."
  return result

removeWithRPM :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
removeWithRPM = execManager . ("rpm -e "++)

removeWithYum :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
removeWithYum = execManager . ("yum remove -y "++)

removeWithAPT :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
removeWithAPT = execManager . ("apt-get remove -y "++)

execManager :: Monad m => String -> CricT m (Either PkgManagerError BL.ByteString)
execManager cmd = do
  result <- exec cmd
  return $ case result of
    Success output    -> Right output
    Error code output -> Left $ UnknownPkgManagerError code output
