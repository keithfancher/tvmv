module File.DirSpec (spec) where

import File.Dir
import Test.Hspec

spec :: Spec
spec = do
  describe "sortCaseInsensitive" $ do
    it "sorts file paths without caring about case" $ do
      sortCaseInsensitive ["test", "something", "Tesz", "Somethinz"]
        `shouldBe` ["something", "Somethinz", "test", "Tesz"]
