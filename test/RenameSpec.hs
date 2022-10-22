module RenameSpec (spec) where

import Rename
import Show (Episode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "generateBaseFileName" $ do
    it "generates the correct base name" $ do
      generateBaseFileName
        ( RenameData
            "Buffy the Vampire Slayer"
            (Episode 10 "Hush" 4)
        )
        `shouldBe` "Buffy the Vampire Slayer - 4x10 - Hush"
