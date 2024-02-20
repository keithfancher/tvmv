module File.NamesSpec (spec) where

import File.Names
import Test.Hspec

spec :: Spec
spec = do
  describe "makePortable" $ do
    it "makes filenames portable and Windows-friendly" $ do
      mapM_ (testResultSet makePortable) portableTestCases

  describe "makeVeryPortable" $ do
    it "makes filenames super, almost annoyingly portable" $ do
      mapM_ (testResultSet makeVeryPortable) veryPortableTestCases

-- Given a tuple of (input, expected output), asserts that the expected result
-- is returned when the input is made valid.
testResultSet :: (FilePath -> FilePath) -> (FilePath, FilePath) -> Expectation
testResultSet testFunc (input, expected) = testFunc input `shouldBe` expected

-- Tuples of (input, expectedOutput)
portableTestCases :: [(FilePath, FilePath)]
portableTestCases =
  [ ("buffy.mkv", "buffy.mkv"), -- already valid, a no-op
    ("buffy: the pilot.mkv", "buffy- the pilot.mkv"),
    ("Do you know 日本語?.mp4", "Do you know ---.mp4"),
    ("nul.mov", "nul_.mov"), -- fun fact: `nul` is reserved in Windows!
    (":::?)_--_.?", "---)_--_."),
    ("Let's go to a café?!", "Let's go to a cafe!"),
    ("Büffÿ: Thë Vämp Slãýêr!!!", "Buffy- The Vamp Slayer!!!"),
    ("BÜffÿ: ThË Vämp SlãÝêr!!!", "BUffy- ThE Vamp SlaYer!!!"), -- mixing case
    ("Æ æ Œ œ oh god why?", "AE ae OE oe oh god why"), -- double chars
    ("François the NIÑO", "Francois the NINO"),
    ("I say \"hello hello\"", "I say 'hello hello'"),
    ("If You Can’t Win, Don’t “Play”", "If You Can't Win, Don't \'Play\'"), -- "smart" quotes
    ("Dover–Calais crossing — and stuff", "Dover-Calais crossing - and stuff") -- em and en dashes
  ]

-- Tuples of (input, expectedOutput)
veryPortableTestCases :: [(FilePath, FilePath)]
veryPortableTestCases =
  [ ("buffy.mkv", "buffy.mkv"), -- still valid, a no-op
    ("buffy: the pilot.mkv", "buffy--the-pilot.mkv"),
    ("Let's go to a café?!", "Let-s-go-to-a-cafe-"),
    ("Do you know 日本語?.mp4", "Do-you-know----.mp4"),
    ("nul.mov", "nul_.mov"), -- still no good!
    (":::?)_--_.?", "----_--_."),
    ("Büffÿ: Thë Vämp Slãýêr!!!", "Buffy--The-Vamp-Slayer---")
  ]
