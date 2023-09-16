module Exec.Match
  ( ParsedFile,
    ParseResults (..),
    getSeasons,
    matchParsedEpisodes,
    parseFilePaths,
  )
where

import Data.Either (partitionEithers)
import Data.List (find)
import Domain.Error (Error (..))
import Domain.Rename (MatchedEpisodes, matchEpisodes)
import Domain.Show (Episode (..))
import Parse (SeasonEpNum (..), parseFilename)

type ParseFailure = FilePath

-- Parsed, which simply means we've extracted and attached its season/ep numbers
type ParsedFile = (FilePath, SeasonEpNum)

data ParseResults = ParseResults
  { successes :: [ParsedFile],
    failures :: [ParseFailure]
  }

-- Attempt to parse the given file paths. Return both successfully parsed files
-- and a list of the failures.
parseFilePaths :: [FilePath] -> ParseResults
parseFilePaths files = ParseResults {successes = s, failures = f}
  where
    (f, s) = partitionEithers $ map parseFile files

-- Extract all the seasons from a list of parsed filenames.
getSeasons :: [ParsedFile] -> [Int]
getSeasons = map season
  where
    season (_, SeasonEpNum s _) = s -- extract season from a single result

-- Match the parsed filenames with corresponding API episode data.
matchParsedEpisodes :: [ParsedFile] -> [Episode] -> Either Error MatchedEpisodes
matchParsedEpisodes results epList = case matchedEps of
  -- TODO: Collect specific filenames which are unmatchable
  Nothing -> Left $ ParseError "Unable to find matching episode data for one or more inputs"
  Just matchedTuples -> makeMatchedEps matchedTuples
  where
    matchedEps = mapM (matchEpisode epList) results

-- Given a single parsed filepath and its season/episode numbers, find and
-- attach its Episode data. Nothing, if there's no matching data.
matchEpisode :: [Episode] -> (FilePath, SeasonEpNum) -> Maybe (FilePath, Episode)
matchEpisode epList (path, seasonEpNum) = (path,) <$> findEp seasonEpNum -- creating a tuple is partially apply-able!
  where
    findEp seasEpNum = find (epsEqual seasEpNum) epList
    epsEqual seasEpNum ep =
      (episodeNum seasEpNum == episodeNumber ep)
        && (seasonNum seasEpNum == episodeSeasonNumber ep)

-- From a list of tuples, build the `MatchedEpisodes` object.
makeMatchedEps :: [(FilePath, Episode)] -> Either Error MatchedEpisodes
makeMatchedEps inTuples = matchEpisodes eps files
  where
    (files, eps) = unzip inTuples

-- If we succeed, we pair the episode data with its `FilePath`. If we fail,
-- return a `Left` of the failed `FilePath`. An easy way to collect failures!
parseFile :: FilePath -> Either ParseFailure ParsedFile
parseFile f = case parseFilename f of
  Left _ -> Left f
  (Right seasonEpNum) -> Right (f, seasonEpNum)
