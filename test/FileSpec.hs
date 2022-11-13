module FileSpec (spec) where

import File
import Test.Hspec

spec :: Spec
spec = do
  describe "normalizeFileList" $ do
    it "sorts and prepends a base to a list of paths" $ do
      normalizeFileList (Just "base") fileList `shouldBe` normalizedWithBase

    it "normalizes (sorts) a list when given no base path" $ do
      normalizeFileList Nothing fileList `shouldBe` normalizedNoBase

  describe "sortCaseInsensitive" $ do
    it "sorts file paths without caring about case" $ do
      sortCaseInsensitive ["test", "something", "Tesz", "Somethinz"]
        `shouldBe` ["something", "Somethinz", "test", "Tesz"]

fileList :: [FilePath]
fileList =
  [ "Zoobob.mp4",
    "Bloobob.mp4",
    "blablah.mp4",
    "asdf.mp4"
  ]

normalizedWithBase :: [FilePath]
normalizedWithBase =
  [ "base/asdf.mp4",
    "base/blablah.mp4",
    "base/Bloobob.mp4",
    "base/Zoobob.mp4"
  ]

normalizedNoBase :: [FilePath]
normalizedNoBase =
  [ "asdf.mp4",
    "blablah.mp4",
    "Bloobob.mp4",
    "Zoobob.mp4"
  ]
