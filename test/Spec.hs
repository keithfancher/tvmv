import qualified RenameSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rename" RenameSpec.spec
