module ParseSpec (spec) where

import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseFilename" $ do
    it "parses all well-formed filenames" $ do
      mapM_ testResultSet inputOutputWellFormed

testResultSet :: (String, SeasonEpNum) -> Expectation
testResultSet (input, expected) = parseFilename input `shouldBe` Right expected

inputOutputWellFormed :: [(String, SeasonEpNum)]
inputOutputWellFormed =
  [ ("1x21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("001x021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- zero-padded
    ("1X21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("s01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("s001e021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- more zeros
    ("S01E21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("S01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}) -- mixed case
  ]
