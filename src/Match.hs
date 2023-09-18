module Match
  ( MatchResults (..),
    ParsedFile,
    ParseResults (..),
    matchParsedEpisodes,
    parseFilePaths,
  )
where

import Data.Either (partitionEithers)
import Data.List (find, nub, sort)
import Domain.Rename (MatchedEpisodes, matchEpisodes')
import Domain.Show (Episode (..))
import Parse (SeasonEpNum (..), parseFilename)

type ParseFailure = FilePath

type MatchFailure = FilePath

-- Parsed, which simply means we've extracted and attached its season/ep numbers
type ParsedFile = (FilePath, SeasonEpNum)

data ParseResults = ParseResults
  { -- Successfully parsed out season and episode numbers, attached to filenames
    successes :: [ParsedFile],
    -- A list of all included seasons in the (successfully parsed) input files
    seasonNumbers :: [Int],
    -- Any file paths that failed to parse are also returned, for error messages, etc.
    failures :: [ParseFailure]
  }
  deriving (Eq, Show)

data MatchResults = MatchResults
  { matchSuccesses :: MatchedEpisodes,
    matchFailures :: [MatchFailure]
  }
  deriving (Eq, Show)

-- Attempt to parse the given file paths. Return both successfully parsed files
-- and a list of the failures.
parseFilePaths :: [FilePath] -> ParseResults
parseFilePaths files = ParseResults {successes = s, failures = f, seasonNumbers = getSeasons s}
  where
    (f, s) = partitionEithers $ map parseFile files

-- Extract all the season numbers from a list of parsed filenames. For your
-- consumption pleasure, they come back sorted.
getSeasons :: [ParsedFile] -> [Int]
getSeasons parsedFiles = sort $ nub $ map seasonNum parsedFiles -- `nub` removes dupes from the list
  where
    seasonNum (_, SeasonEpNum s _) = s -- extract season number from a single result

-- Match the parsed filenames with corresponding API episode data.
matchParsedEpisodes :: [ParsedFile] -> [Episode] -> MatchResults
matchParsedEpisodes results epList =
  MatchResults
    { matchSuccesses = matchEpisodes' matchedTuples,
      matchFailures = matchFailures
    }
  where
    (matchFailures, matchedTuples) = partitionEithers $ map (matchEpisode epList) results

-- Given a single parsed filepath and its season/episode numbers, find and
-- attach its Episode data. A `Left` if there's no matching data.
matchEpisode :: [Episode] -> ParsedFile -> Either MatchFailure (Episode, FilePath)
matchEpisode epList (path, seasonEpNum) = (,path) <$> findEp seasonEpNum -- tuple creation is partially apply-able!
  where
    findEp seasEpNum = case find (epsEqual seasEpNum) epList of
      Nothing -> Left path -- send back the path so we can collect failures easily
      Just ep -> Right ep
    epsEqual seasEpNum ep =
      (episodeNum seasEpNum == episodeNumber ep)
        && (seasonNum seasEpNum == episodeSeasonNumber ep)

-- If we succeed, we pair the episode data with its `FilePath`. If we fail,
-- return a `Left` of the failed `FilePath`. An easy way to collect failures!
parseFile :: FilePath -> Either ParseFailure ParsedFile
parseFile f = case parseFilename f of
  Left _ -> Left f
  (Right seasonEpNum) -> Right (f, seasonEpNum)
