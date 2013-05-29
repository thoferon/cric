import Test.Hspec

import qualified Cric.CoreSpec as CricCore
import qualified Cric.FileSystemSpec as FileSystem

main :: IO ()
main = hspec $ do
  CricCore.test
  FileSystem.test
