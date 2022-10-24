import qualified FileSpec
import qualified RenameSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "File" FileSpec.spec
  describe "Rename" RenameSpec.spec
