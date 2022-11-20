import qualified API.TMDBSpec
import qualified Exec.EnvSpec
import qualified FileSpec
import qualified LogSpec
import qualified RenameSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "API.TMDB" API.TMDBSpec.spec
  describe "Exec.Env" Exec.EnvSpec.spec
  describe "File" FileSpec.spec
  describe "Log" LogSpec.spec
  describe "Rename" RenameSpec.spec
