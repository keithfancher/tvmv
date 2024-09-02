module ParseSpec (spec) where

import Data.Either (isLeft)
import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseFilename" $ do
    it "parses all well-formed base cases (season + ep num ONLY)" $ do
      mapM_ testResultSetEpNumsOnly inputOutputBaseCases

    it "parses with trailing characters, ignoring them" $ do
      mapM_ testResultSetEpNumsOnly inputOutputTrailing

    it "parses all well-formed full filenames" $ do
      mapM_ testResultSetEpNumsOnly inputOutputFullFilename

    it "parses all well-formed full paths" $ do
      mapM_ testResultSetEpNumsOnly inputOutputFullPath

    it "parses out show names, when they exist" $ do
      mapM_ testResultSet inputOutputShowNames

    it "fails when given episode ranges, which are not yet supported" $ do
      mapM_ testFailures inputOutputEpRanges

-- Given a tuple of (input, expected output), asserts that the expected result
-- is returned when the input is parsed.
testResultSet :: (String, EpisodeData) -> Expectation
testResultSet (input, expected) = parseFilename input `shouldBe` Right expected

-- Same as above, but only asserting on season and episode numbers.
testResultSetEpNumsOnly :: (String, SeasonEpNum) -> Expectation
testResultSetEpNumsOnly (input, expected) = (seasonEpNum <$> parseFilename input) `shouldBe` Right expected

testFailures :: String -> Expectation
testFailures input = parseFilename input `shouldSatisfy` isLeft

inputOutputBaseCases :: [(String, SeasonEpNum)]
inputOutputBaseCases =
  [ ("1x21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("001x021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- zero-padded
    ("1X21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("s01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("s001e021", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- more zeros
    ("S01E21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- uppercase
    ("S01e21", SeasonEpNum {seasonNum = 1, episodeNum = 21}), -- mixed case
    ("ep12", SeasonEpNum {seasonNum = 1, episodeNum = 12}), -- ep-number only
    ("eP12", SeasonEpNum {seasonNum = 1, episodeNum = 12}) -- ep-number only, mixed case
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

inputOutputFullPath :: [(String, SeasonEpNum)]
inputOutputFullPath =
  [ ("/home/jimbo/tv/A show - 1x21 - An episode.mkv", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("/media/0x12dumbdirectory/Poirot - s035e123 - s12e04 baaad.mp4", SeasonEpNum {seasonNum = 35, episodeNum = 123})
  ]

inputOutputShowNames :: [(String, EpisodeData)]
inputOutputShowNames =
  [ ( "Buffy - 1x21 - An episode.mkv",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 21}) (Just "Buffy")
    ),
    ( "/media/0x12dumbdirectory/Poirot - s035e123 - s12e04 baaad.mp4",
      EpisodeData (SeasonEpNum {seasonNum = 35, episodeNum = 123}) (Just "Poirot")
    ),
    ( "Angel.S01E03.In.The.Dark.mp4",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 3}) (Just "Angel")
    ),
    ( "Psycho-Pass-EP03-blah blah blah",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 3}) (Just "Psycho-Pass")
    ),
    ( "samurai.gourmet.s01e12.720p.mkv",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 12}) (Just "samurai gourmet")
    ),
    ( "Californication [2x01].avi",
      EpisodeData (SeasonEpNum {seasonNum = 2, episodeNum = 1}) (Just "Californication")
    ),
    ( "Higurashi - When They Cry - s01e01 - The Hidden Demon Chapter - Part 1 - The Beginning.mkv",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 1}) (Just "Higurashi - When They Cry")
    ),
    ( "Steins;Gate - EP01 - Turning Point.mkv",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 1}) (Just "Steins;Gate")
    ),
    ( "1x21 - An episode.mp4",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 21}) Nothing
    ),
    ( "        ?_++//[]]]]]]]]]1x21 - An episode.mp4",
      EpisodeData (SeasonEpNum {seasonNum = 1, episodeNum = 21}) Nothing
    )
  ]

-- These all use episode ranges, which are not (yet) supported:
inputOutputEpRanges :: [String]
inputOutputEpRanges =
  [ "Adventure Time - S06E01-E02.mp4",
    "BSG s04e19-21.mkv", -- missing the second "e", but we still match it
    "BSG s04e19-21 s03e4.mkv", -- a mish-mash
    "/home/jimbo/tv/A show - s01e21-E200 - An episode.mkv"
  ]
