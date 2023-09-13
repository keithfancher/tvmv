import API.TMDBSpec qualified
import Domain.RenameSpec qualified
import Exec.EnvSpec qualified
import FileSpec qualified
import LogSpec qualified
import ParseSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "API.TMDB" API.TMDBSpec.spec
  describe "Domain.Rename" Domain.RenameSpec.spec
  describe "Exec.Env" Exec.EnvSpec.spec
  describe "File" FileSpec.spec
  describe "Log" LogSpec.spec
  describe "Parse" ParseSpec.spec
