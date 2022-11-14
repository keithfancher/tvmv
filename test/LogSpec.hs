module LogSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Log
import Rename (RenameOp (..), RenameResult (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "getLogText" $ do
    it "converts successful ops to text, filtering out failures" $ do
      getLogText renameResults `shouldBe` successesAsText

    it "gets valid text even with empty results" $ do
      getLogText [] `shouldBe` "[]"

  describe "renameOpsFromText" $ do
    it "correctly parses an empty list" $ do
      renameOpsFromText "[]" `shouldBe` Right []

    it "correctly parses a valid list of ops" $ do
      renameOpsFromText validOpsText `shouldBe` Right validOpsParsed

    it "returns a Left in the case of a parsing error" $ do
      renameOpsFromText "wut oh hey I'm not valid" `shouldSatisfy` isLeft

renameResults :: [RenameResult]
renameResults =
  [ RenameResult {op = opEp1, success = True},
    RenameResult {op = opEp2, success = False}
  ]

successesAsText :: T.Text
successesAsText = "[RenameOp {oldPath = \"test/data/Poirot S12E1.mp4\", newPath = \"test/data/Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4\"}]"

validOpsText :: T.Text
validOpsText = "[RenameOp {oldPath = \"test/data/Poirot S12E1.mp4\", newPath = \"test/data/Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4\"},RenameOp {oldPath = \"test/data/Poirot S12E2.mp4\", newPath = \"test/data/Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4\"}]"

validOpsParsed :: [RenameOp]
validOpsParsed = [opEp1, opEp2]

opEp1 :: RenameOp
opEp1 =
  RenameOp
    { oldPath = "test/data/Poirot S12E1.mp4",
      newPath = "test/data/Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4"
    }

opEp2 :: RenameOp
opEp2 =
  RenameOp
    { oldPath = "test/data/Poirot S12E2.mp4",
      newPath = "test/data/Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4"
    }
