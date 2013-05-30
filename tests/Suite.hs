import Test.Hspec

import qualified Cric.CoreSpec as CricCore
import qualified Cric.FileSystemSpec as FileSystem
import qualified Cric.PackagesSpec as Packages
import qualified Cric.SystemInfoSpec as SystemInfo
import qualified Cric.UsersSpec as Users

main :: IO ()
main = hspec $ do
  CricCore.test
  FileSystem.test
  Packages.test
  SystemInfo.test
  Users.test
