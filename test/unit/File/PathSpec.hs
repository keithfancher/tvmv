module File.PathSpec (spec) where

import File.Path
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "replaceBaseName'" $ do
    it "replaces base file names, accounting for subtitle language metadata" $ do
      mapM_ testResultSet testCases

testResultSet :: (FilePath, String, FilePath) -> Expectation
testResultSet (inFile, replacement, expected) = replaceBaseName' inFile replacement `shouldBe` expected

-- Tuples of (inputFilePath, replacement, expectedOutput)
testCases :: [(FilePath, String, FilePath)]
testCases =
  [ ("foo.srt", "bar", "bar.srt"),
    ("foo.en.srt", "bar", "bar.en.srt"),
    ("stuff" </> "foo.en.sdh.forced.srt", "bar", "stuff" </> "bar.en.sdh.forced.srt"),
    -- Stops at the first invalid case ("butt"), even if there are more valid cases beyond that:
    ("foo.forced.in.butt.de.sdh.srt", "bar", "bar.de.sdh.srt"),
    -- Note that our func will keep grabbing more as long as it can, regardless
    -- of whether it makes sense. (Multiple languages, repeats, etc.)
    ("foo.de.en.cc.sdh.cc.forced.en", "bar", "bar.de.en.cc.sdh.cc.forced.en"),
    -- No false positives, checks for valid language codes:
    ("foo.cc.forced.poo.xx.srt", "bar", "bar.srt")
  ]
