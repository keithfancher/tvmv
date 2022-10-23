module RenameSpec (spec) where

import Rename
import Show (Episode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "renameFile" $ do
    it "generates the correct file name" $ do
      renameFile hushData "/root/buffy/season 4/buff 4.10.mkv"
        `shouldBe` RenameOp
          { oldPath = "/root/buffy/season 4/buff 4.10.mkv",
            newPath = "/root/buffy/season 4/Buffy the Vampire Slayer - 4x10 - Hush.mkv"
          }

hushData :: RenameData
hushData = RenameData {showName = "Buffy the Vampire Slayer", episode = hush}
  where
    hush = Episode {episodeNumber = 10, episodeName = "Hush", episodeSeasonNumber = 4}
