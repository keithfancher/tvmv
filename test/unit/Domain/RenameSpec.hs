module Domain.RenameSpec (spec) where

import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Domain.Rename
import Domain.Show (Episode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "matchEpisodes" $ do
    it "returns an error value if given mismatched number of files/episodes" $ do
      matchEpisodes [welcomeEp] ["two filenames", "but one ep"] `shouldSatisfy` isLeft

    -- Note that we *can't* construct a value to assert against here, as the
    -- `MatchedEpisodes` data constructor is not exposed. Our `renameFiles`
    -- tests will also cover this, however.
    it "returns a success value if the input lists are the same length" $ do
      matchEpisodes [welcomeEp, hushEp] ["two filenames", "and two eps"] `shouldSatisfy` isRight

  describe "matchEpisodesAllowPartial" $ do
    it "allows a match if there are more episodes than files" $ do
      matchEpisodesAllowPartial [welcomeEp, hushEp] ["only one file"] `shouldSatisfy` isRight

    it "allows an empty list of files" $ do
      matchEpisodesAllowPartial [welcomeEp, hushEp] [] `shouldSatisfy` isRight

    it "still returns an error value if there are more files than episodes" $ do
      matchEpisodesAllowPartial [welcomeEp] ["two filenames", "but one ep"] `shouldSatisfy` isLeft

  describe "renameFiles" $ do
    it "generates the correct file names" $ do
      let matchedEps = matchEpisodes [welcomeEp, hushEp] welcomeHushFileNames
      renameFiles <$> matchedEps `shouldBe` Right expectedRenameOps

    it "returns an empty list if given empty lists" $ do
      renameFiles <$> matchEpisodes [] [] `shouldBe` Right []

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

welcomeHushFileNames :: [FilePath]
welcomeHushFileNames = ["/root/buffy/season 1/buff 1.1.mkv", "/root/buffy/season 4/buff 4.10.mkv"]

expectedRenameOps :: [RenameOp]
expectedRenameOps =
  [ RenameOp
      { oldPath = "/root/buffy/season 1/buff 1.1.mkv",
        newPath = "/root/buffy/season 1/Buffy the Vampire Slayer - 1x01 - Welcome to the Hellmouth.mkv"
      },
    RenameOp
      { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
        newPath = "/root/buffy/season 4/Buffy the Vampire Slayer - 4x10 - Hush.mkv"
      }
  ]

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
