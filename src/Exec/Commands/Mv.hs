module Exec.Commands.Mv (mv) where

import API qualified
import Command (MvOptions (..), SearchKey (..), SeasonSelection (..))
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Data.List (intercalate)
import Data.Text qualified as T
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (MatchedEpisodes, episodes, matchEpisodes, matchEpisodesAllowPartial, renameFiles)
import Domain.Show (Episode (..), Season (..))
import Exec.Env (Env, populateAPIKey)
import Exec.Filter (filterFiles)
import Exec.Rename (RenameResult, runRenameOps)
import File (listFiles)
import Filenames (makePortable)
import Match (MatchResults (..), ParseResults (..), ParsedFile, matchParsedEpisodes, parseFilePaths)
import System.Directory (makeRelativeToCurrentDirectory)
import Text.Printf (printf)

-- Rename the files of a TV season. This puts the `mv` in tvmv!
mv ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  MvOptions ->
  m ()
mv env withApi mvOptions = do
  let (MvOptions maybeApiKey force _noLog _partial mkPortable _searchKey seasonSelection inFiles) = mvOptions

  -- Before doing anything, ensure we have an API key available:
  apiKey <- liftEither $ populateAPIKey maybeApiKey env

  -- Get the list of input files, parse out relevant season/episode data:
  filteredFiles <- liftIO $ listFiles inFiles >>= filterFiles
  let parseResults = parseFilePaths filteredFiles
  showParseFailures seasonSelection parseResults
  seasonNums <- getSeasonNums seasonSelection parseResults

  -- Fetch episode data from our configured API:
  putStrLn' $ showFetchMessage seasonNums
  episodeData <- fetchEpisodeData (searchSeason apiKey) seasonNums

  -- If the option is set, make episode names "portable", aka Windows-friendly:
  let niceEpData = if mkPortable then makePortableEpNames episodeData else episodeData

  -- Match API data with the list of input files, smartly or dumbly:
  matchedFiles <- case seasonSelection of
    SeasonNum _ -> liftEither $ match niceEpData filteredFiles -- lexicographic sort, "dumb" matching
    Auto -> autoMatchFiles (successes parseResults) niceEpData -- parsed filenames, "smart" matching

  -- Finally, actually rename the input files based on the API data:
  let renameOps = renameFiles matchedFiles
  runRenameOps renameOps (renameMsg renameOps) force
  where
    match = if allowPartial mvOptions then matchEpisodesAllowPartial else matchEpisodes
    renameMsg f = printf "Preparing to execute the following %d rename operations...\n" (length f)
    searchSeason k = case searchKey mvOptions of
      (Name n) -> API.searchSeasonByName withApi k n
      (Id i) -> API.searchSeasonById withApi k i

-- Slightly neurotic? Nicer printing of the input seasons.
showFetchMessage :: [Int] -> String
showFetchMessage seasons = case seasons of
  [s] -> base <> " " <> show s -- e.g. "season 12"
  [f, s] -> base <> "s " <> show f <> " and " <> show s -- e.g. "seasons 12 and 13"
  threeOrMore -> base <> "s " <> withCommas threeOrMore -- e.g. "seasons 12, 13, 14"
  where
    withCommas l = intercalate ", " $ map show l
    base = "Fetching episode data from API for season"

-- Fetch data for the given seasons via the given API function, concat all
-- episodes of the resulting seasons into a single list.
fetchEpisodeData :: (Monad m) => (Int -> m Season) -> [Int] -> m [Episode]
fetchEpisodeData apiGetSeason seasonNumList = concatSeasonEps <$> mapM apiGetSeason seasonNumList
  where
    concatSeasonEps = concatMap Domain.Show.episodes

-- If there are any failed matches, we show them to the user but continue on.
-- If we get ZERO successful matches, however, the whole operation is a failure.
autoMatchFiles :: (MonadIO m, MonadError Error m) => [ParsedFile] -> [Episode] -> m MatchedEpisodes
autoMatchFiles parsedFiles epList = do
  let (MatchResults matchedEps f) = matchParsedEpisodes parsedFiles epList
  showMatchFailures f
  case Domain.Rename.episodes matchedEps of -- stupid ambiguous refs require this qualification :')
    [] -> throwError $ ParseError zeroMatchError
    _nonEmpty -> return matchedEps
  where
    zeroMatchError =
      "No parsed files matched the API episode data ... try using the `-s` flag to manually specify the correct season?"
    matchError = "\nFailed to match the following files with corresponding API episode data:"
    showMatchFailures [] = return ()
    showMatchFailures failures = printRelativeFiles failures matchError

showParseFailures :: (MonadIO m) => SeasonSelection -> ParseResults -> m ()
showParseFailures (SeasonNum _) _ = return () -- No parsing means no failures
showParseFailures Auto (ParseResults _ _ []) = return () -- No failures also means no failures!
showParseFailures Auto (ParseResults _ _ failures) = printRelativeFiles failures errMsg
  where
    errMsg = "Failed to parse season/episode numbers from the following files:"

-- A helper to print a newline-delimited string of relative paths, given a list
-- of FilePaths. With a message at the top!
printRelativeFiles :: (MonadIO m) => [FilePath] -> String -> m ()
printRelativeFiles files msg = do
  putStrLn' msg
  f <- liftIO withNewLines
  putStrLn' $ f <> "\n"
  where
    withNewLines = intercalate "\n" <$> relativeFiles
    relativeFiles = mapM makeRelativeToCurrentDirectory files

-- If the user has specified a season, simply return that. In the case of
-- auto-detection, pull the seasons from the parsed input file list.
getSeasonNums :: (MonadError Error m) => SeasonSelection -> ParseResults -> m [Int]
getSeasonNums (SeasonNum n) _ = return [n]
getSeasonNums Auto parseResults = case seasonNumbers parseResults of
  [] -> throwError $ ParseError "Unable to parse season/episode data from any input files"
  nonEmpty -> return nonEmpty

-- Make episode names "portable", aka Windows-friendly. Remove/replace fancy
-- characters, watch for reserved filenames, etc.
makePortableEpNames :: [Episode] -> [Episode]
makePortableEpNames = map makePortableName
  where
    makePortableName e = e {episodeName = makeTextPortable (episodeName e)}
    makeTextPortable = T.pack . makePortable . T.unpack

-- Wrapper for less lifting :')
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn
