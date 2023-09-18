module MatchSpec (spec) where

import Domain.Rename (matchEpisodes')
import Domain.Show (Episode (..))
import Match
import Parse (SeasonEpNum (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseFilePaths" $ do
    it "parses successes and failures from input file paths" $ do
      parseFilePaths inputFiles `shouldBe` parsedFiles

  describe "matchParsedEpisodes" $ do
    -- TODO: test failures!
    it "matches successfully parsed filenames with episode API data" $ do
      matchParsedEpisodes (successes parsedFiles) episodeData `shouldBe` matchedEpisodes

inputFiles :: [FilePath]
inputFiles =
  [ "Poirot - 1x23 - He catches a cold.mkv",
    "Some un-parseable jibber-jabber",
    "Poirot s12e4.mp4",
    "Poirot s003e19 - Shut up, Hastings.mp4"
  ]

parsedFiles :: ParseResults
parsedFiles =
  ParseResults
    { successes =
        [ ( "Poirot - 1x23 - He catches a cold.mkv",
            SeasonEpNum {seasonNum = 1, episodeNum = 23}
          ),
          ( "Poirot s12e4.mp4",
            SeasonEpNum {seasonNum = 12, episodeNum = 4}
          ),
          ( "Poirot s003e19 - Shut up, Hastings.mp4",
            SeasonEpNum {seasonNum = 3, episodeNum = 19}
          )
        ],
      seasonNumbers = [1, 3, 12],
      failures = ["Some un-parseable jibber-jabber"]
    }

-- Note that only the episode and seasons numbers matter here, really. Also
-- note that we (intentionally) have more episodes returned than there are
-- files. The extra "API data" here should just be ignored!
episodeData :: [Episode]
episodeData =
  [ Episode {episodeSeasonNumber = 1, episodeNumber = 23, episodeName = "He catches a cold", episodeShowName = ""},
    Episode {episodeSeasonNumber = 12, episodeNumber = 4, episodeName = "", episodeShowName = ""},
    Episode {episodeSeasonNumber = 3, episodeNumber = 19, episodeName = "Shut up, Hastings", episodeShowName = ""},
    Episode {episodeSeasonNumber = 11, episodeNumber = 1, episodeName = "An extra episode", episodeShowName = ""}
  ]

-- This looks a little strange because of the private constructor. We have to
-- use our own match function to build this object for comparison.
matchedEpisodes :: MatchResults
matchedEpisodes =
  MatchResults
    { matchSuccesses = matchEpisodes' $ zip episodeData matchedFilePaths,
      matchFailures = []
    }

matchedFilePaths :: [FilePath]
matchedFilePaths =
  [ "Poirot - 1x23 - He catches a cold.mkv",
    "Poirot s12e4.mp4",
    "Poirot s003e19 - Shut up, Hastings.mp4"
  ]
