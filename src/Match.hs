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
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Domain.Rename (MatchedEpisodes, matchEpisodes')
import Domain.Show (Episode (..))
import Parse (EpisodeData (..), SeasonEpNum (..), parseFilename)

type ParseFailure = FilePath

type MatchFailure = FilePath

type ShowName = Text

-- Parsed, which simply means we've extracted and attached its season/ep
-- numbers, and potentially its show name.
type ParsedFile = (FilePath, SeasonEpNum, Maybe ShowName)

data ParseResults = ParseResults
  { -- Successfully parsed out season and episode numbers, attached to filenames
    successes :: [ParsedFile],
    -- A list of all included seasons in the (successfully parsed) input files
    seasonNumbers :: [Int],
    -- A list of all included show names in the parsed input files
    showNames :: [ShowName],
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
parseFilePaths files =
  ParseResults
    { successes = s,
      failures = f,
      showNames = getShowNames s,
      seasonNumbers = getSeasons s
    }
  where
    (f, s) = partitionEithers $ map parseFile files

-- Extract all the season numbers from a list of parsed filenames.
getSeasons :: [ParsedFile] -> [Int]
getSeasons parsedFiles = sortAndDeDupe $ map seasonNum parsedFiles
  where
    seasonNum (_, SeasonEpNum s _, _) = s -- extract season number from a single result

-- Extract all the show names from a list of parsed filenames. `Nothing`s are
-- simply filtered out.
getShowNames :: [ParsedFile] -> [ShowName]
getShowNames parsedFiles = sortAndDeDupe $ mapMaybe getName parsedFiles
  where
    getName (_, _, maybeName) = maybeName -- extract season number from a single result

sortAndDeDupe :: (Ord a) => [a] -> [a]
sortAndDeDupe = sort . nub -- `nub` removes dupes from the list

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
matchEpisode epList (path, seasonEpNum, _name) = (,path) <$> findEp seasonEpNum -- tuple creation is partially apply-able!
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
  Right (EpisodeData seasonEpNum maybeName) -> Right (f, seasonEpNum, maybeName)
