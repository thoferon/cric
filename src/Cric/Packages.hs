module Cric.Packages (
  PackageManager(..), PkgInstallError(..)
  , getPackageManager
  , installPackage
  , installWithRPM, installWithYum, installWithAPT
  ) where

import Prelude hiding (log)

import Cric

import qualified Data.ByteString.Lazy as BL

data PackageManager = RPM | Yum | APT
                    | UnknownPackageManager
                    deriving (Show, Eq)

data PkgInstallError = NoPackageManagerFound
                     | UnknownPkgInstallError Int BL.ByteString
                     deriving (Show, Eq)

getPackageManager :: Cric PackageManager
getPackageManager = getPackageManager' [("rpm", RPM), ("yum", Yum), ("apt-get", APT)]
  where
    getPackageManager' :: [(String, PackageManager)] -> Cric PackageManager
    getPackageManager' [] = return UnknownPackageManager
    getPackageManager' ((cmd,mgr):rest) = do
      test <- testCommand cmd
      case test of
        True -> return mgr
        False -> getPackageManager' rest

installPackage :: String -> Cric (Either PkgInstallError BL.ByteString)
installPackage pkgName = do
  log LInfo $ "Installing " ++ pkgName ++ " ..."
  pkgMgr <- getPackageManager
  result <- case pkgMgr of
    RPM -> installWithRPM pkgName
    Yum -> installWithYum pkgName
    APT -> installWithAPT pkgName
    UnknownPackageManager -> return $ Left NoPackageManagerFound
  case result of
    Left err -> log LError $ "Error installing " ++ pkgName ++ " (" ++ show err ++ ")"
    Right _  -> log LInfo  $ "Package " ++ pkgName ++ " installed successfully."
  return result

installWithRPM :: String -> Cric (Either PkgInstallError BL.ByteString)
installWithRPM = installWithCommand . ("rpm -i "++)

installWithYum :: String -> Cric (Either PkgInstallError BL.ByteString)
installWithYum = installWithCommand . ("yum install -y "++)

installWithAPT :: String -> Cric (Either PkgInstallError BL.ByteString)
installWithAPT = installWithCommand . ("apt-get install -y "++)

installWithCommand :: String -> Cric (Either PkgInstallError BL.ByteString)
installWithCommand cmd = do
  result <- exec cmd
  return $ case result of
    Success output -> Right output
    Error code output -> Left $ UnknownPkgInstallError code output
