import qualified ExecuteSpec
import qualified FileSpec
import qualified LogSpec
import qualified RenameSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Execute" ExecuteSpec.spec
  describe "File" FileSpec.spec
  describe "Log" LogSpec.spec
  describe "Rename" RenameSpec.spec
