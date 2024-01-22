module FilenamesSpec (spec) where

import Filenames
import Test.Hspec

spec :: Spec
spec = do
  describe "makePortable" $ do
    it "make filenames portable and Windows-friendly" $ do
      mapM_ (testResultSet makePortable) portableTestCases

-- Given a tuple of (input, expected output), asserts that the expected result
-- is returned when the input is made valid.
testResultSet :: (FilePath -> FilePath) -> (FilePath, FilePath) -> Expectation
testResultSet testFunc (input, expected) = testFunc input `shouldBe` expected

portableTestCases :: [(FilePath, FilePath)]
portableTestCases =
  [ ("buffy.mkv", "buffy.mkv"), -- already valid, a no-op
    ("buffy: the pilot.mkv", "buffy_ the pilot.mkv"),
    ("Let's go to a café?!", "Let's go to a caf-_!"),
    ("Do you know 日本語?.mp4", "Do you know ---_.mp4"),
    ("nul.mov", "nul_.mov"), -- fun fact: `nul` is reserved in Windows!
    (":::?)_--_.?", "____)_--_._")
  ]
