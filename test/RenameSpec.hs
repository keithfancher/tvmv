module RenameSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Rename
import Show (Episode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "renameFiles" $ do
    it "generates the correct file names" $ do
      renameFiles
        [welcomeEp, hushEp]
        ["/root/buffy/season 1/buff 1.1.mkv", "/root/buffy/season 4/buff 4.10.mkv"]
        `shouldBe` Right
          [ RenameOp
              { oldPath = "/root/buffy/season 1/buff 1.1.mkv",
                newPath = "/root/buffy/season 1/Buffy the Vampire Slayer - 1x01 - Welcome to the Hellmouth.mkv"
              },
            RenameOp
              { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
                newPath = "/root/buffy/season 4/Buffy the Vampire Slayer - 4x10 - Hush.mkv"
              }
          ]

    it "returns an empty list if given empty lists" $ do
      renameFiles [] [] `shouldBe` Right []

    it "returns an error value if given mismatched number of files/episodes" $ do
      renameFiles [welcomeEp] ["two filesnames", "but one ep"] `shouldSatisfy` isLeft

  describe "renameFile" $ do
    it "generates the correct file name" $ do
      renameFile hushEp "/root/buffy/season 4/buff 4.10.mkv"
        `shouldBe` RenameOp
          { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
            newPath = "/root/buffy/season 4/Buffy the Vampire Slayer - 4x10 - Hush.mkv"
          }

  describe "undoRenameOp" $ do
    it "flip the old/new args for a given op" $ do
      undoRenameOp RenameOp {oldPath = "old/path/to/file.mkv", newPath = "new/path/to/file.mkv"}
        `shouldBe` RenameOp
          { oldPath = "new/path/to/file.mkv",
            newPath = "old/path/to/file.mkv"
          }

buffy :: T.Text
buffy = "Buffy the Vampire Slayer"

welcomeEp :: Episode
welcomeEp =
  Episode
    { episodeNumber = 1,
      episodeName = "Welcome to the Hellmouth",
      episodeSeasonNumber = 1,
      episodeShowName = buffy
    }

hushEp :: Episode
hushEp =
  Episode
    { episodeNumber = 10,
      episodeName = "Hush",
      episodeSeasonNumber = 4,
      episodeShowName = buffy
    }
