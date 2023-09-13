module ParseSpec (spec) where

import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseFilename" $ do
    it "parses all well-formed base cases (season + ep num ONLY)" $ do
      mapM_ testResultSet inputOutputBaseCases

    it "parses with trailing characters, ignoring them" $ do
      mapM_ testResultSet inputOutputTrailing

    it "parses all well-formed full filenames" $ do
      mapM_ testResultSet inputOutputFullFilename

testResultSet :: (String, SeasonEpNum) -> Expectation
testResultSet (input, expected) = parseFilename input `shouldBe` Right expected

inputOutputBaseCases :: [(String, SeasonEpNum)]
inputOutputBaseCases =
  [ ("1x21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("001x021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- zero-padded
    ("1X21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("s01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("s001e021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- more zeros
    ("S01E21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("S01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}) -- mixed case
  ]

inputOutputTrailing :: [(String, SeasonEpNum)]
inputOutputTrailing =
  [ ("1x21anythingelseafterthis", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("s01e21 - an ep or something", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("s001e021 and then a contradiction s03e05", SeasonEpNum {seasonNum = 1, episodeNum = 21})
  ]

inputOutputFullFilename :: [(String, SeasonEpNum)]
inputOutputFullFilename =
  [ ("A pretty cool TV show - 1x21 - An alright episode", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("Buffy - s09e99 - Buffy's wedding", SeasonEpNum {seasonNum = 9, episodeNum = 99}),
    ("Poirot - s035e123 - A confusing s12e04 episode title", SeasonEpNum {seasonNum = 35, episodeNum = 123})
  ]
