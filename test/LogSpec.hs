module LogSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Log
import Rename (RenameOp (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "renameOpsFromText" $ do
    it "correctly parses an empty list" $ do
      renameOpsFromText "[]" `shouldBe` Right []

    it "correctly parses a valid list of ops" $ do
      renameOpsFromText validOpsText `shouldBe` Right validOpsParsed

    it "returns a Left in the case of a parsing error" $ do
      renameOpsFromText "wut oh hey I'm not valid" `shouldSatisfy` isLeft

validOpsText :: T.Text
validOpsText = "[RenameOp {oldPath = \"test/data/Poirot S12E1.mp4\", newPath = \"test/data/Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4\"},RenameOp {oldPath = \"test/data/Poirot S12E2.mp4\", newPath = \"test/data/Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4\"}]"

validOpsParsed :: [RenameOp]
validOpsParsed =
  [ RenameOp
      { oldPath = "test/data/Poirot S12E1.mp4",
        newPath = "test/data/Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4"
      },
    RenameOp
      { oldPath = "test/data/Poirot S12E2.mp4",
        newPath = "test/data/Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4"
      }
  ]
