module LogSpec (spec) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Domain.Rename (RenameOp (..))
import Exec.Rename (RenameResult (..))
import Log
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

  describe "getLatestLog" $ do
    it "gets the most recent log file, if one exists" $ do
      getLatestLog fileListWithLogs `shouldBe` Just "tvmv-log-123470.txt"

    it "returns Nothing if no log files are present" $ do
      getLatestLog fileListNoLogs `shouldBe` Nothing

renameResults :: [RenameResult]
renameResults =
  [ Success opEp1,
    Failure opEp2 (userError "Permission denied or something")
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

fileListWithLogs :: [FilePath]
fileListWithLogs =
  [ "Poirot S12E1.mp4",
    "tvmv-log-123456.txt",
    "Poirot S12E2.mp4",
    "tvmv-log-123.txt",
    "Poirot S12E3.mp4",
    "tvmv-log-123470.txt", -- most recent
    "Poirot S12E4.mp4"
  ]

fileListNoLogs :: [FilePath]
fileListNoLogs =
  [ "Poirot S12E1.mp4",
    "Poirot S12E2.mp4",
    "Poirot S12E3.mp4",
    "Poirot S12E4.mp4"
  ]
