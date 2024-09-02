module Exec.Commands.Mv (mv) where

import API qualified
import Command (MvOptions (..), SearchKey (..), UserSearchTerms (..))
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Data.List (intercalate, intersperse)
import Data.Text qualified as T
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (MatchedEpisodes, episodes, matchEpisodes, matchEpisodesAllowPartial, renameFiles)
import Domain.Show (Episode (..), Season (..))
import Exec.Env (Env)
import Exec.Filter (filterFiles)
import Exec.Rename (RenameResult, runRenameOps)
import File.Dir (listFiles)
import File.Names (makePortable)
import Match (MatchResults (..), ParseResults (..), ParsedFile, matchParsedEpisodes, parseFilePaths)
import Print.Color (ColorText, asWarning, cyan, printColorLn)
import System.Directory (makeRelativeToCurrentDirectory)

-- Rename the files of a TV season. This puts the `mv` in tvmv!
mv ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  MvOptions ->
  m ()
mv env withApi mvOptions = do
  let (MvOptions maybeApiKey force _noLog _partial unicodeFilenames userSearchTerms inFiles) = mvOptions

  -- Before doing anything, ensure we have an API key available:
  apiKey <- API.resolveAPIKey maybeApiKey env

  -- Get the list of input files, parse out relevant season/episode data:
  filteredFiles <- liftIO $ listFiles inFiles >>= filterFiles
  let parseResults = parseFilePaths filteredFiles

  -- We need "search type" to know whether we actually care about parse
  -- failures or not. (If we're not parsing, we don't care!)
  let searchType = getSearchType userSearchTerms
  showParseFailures searchType parseResults
  let seasonSelection' = userSearchTerms >>= seasonSelection
  seasonNums <- getSeasonNums seasonSelection' parseResults

  -- Fetch episode data from our configured API:
  printColorLn $ showFetchMessage seasonNums
  episodeData <- fetchEpisodeData (searchSeason apiKey) seasonNums

  -- By default we make episode names "portable", aka Windows-friendly:
  let niceEpData = if unicodeFilenames then episodeData else makePortableEpNames episodeData

  -- Match API data with the list of input files, smartly or dumbly, based on
  -- whether the user has provided a season or we've parsed it out:
  matchedFiles <- case seasonSelection' of
    Just _ -> liftEither $ match niceEpData filteredFiles -- lexicographic sort, "dumb" matching
    Nothing -> autoMatchFiles (successes parseResults) niceEpData -- parsed filenames, "smart" matching

  -- Finally, actually rename the input files based on the API data:
  let renameOps = renameFiles matchedFiles
  runRenameOps renameOps (renameMsg renameOps) force
  where
    match = if allowPartial mvOptions then matchEpisodesAllowPartial else matchEpisodes
    renameMsg ops =
      "Preparing to execute the following " <> colorNum (length ops) <> " rename operations...\n"
    searchSeason k = case showSelection <$> userSearchTerms mvOptions of
      (Just (Name n)) -> API.searchSeasonByName withApi k n
      (Just (Id i)) -> API.searchSeasonById withApi k i
      Nothing -> error "TODO! Show-name parsing not implemented yet :(" -- TODO!

-- Based on what data the user does and doesn't provide, we determine what
-- extra bits need parsing out, if any. This extra bit of mapping here (and
-- `getSearchType` below) means the user shouldn't ever have to explicitly
-- select a "mode" of operation -- "auto" or what have you -- they just give us
-- what they have and we do the rest.
data SearchType = ParseNameAndSeason | ParseSeason | NoParse

getSearchType :: Maybe UserSearchTerms -> SearchType
getSearchType Nothing = ParseNameAndSeason -- nothing provided, must parse everything
getSearchType (Just (UserSearchTerms _ Nothing)) = ParseSeason -- search key provided but no season
getSearchType (Just (UserSearchTerms _ (Just _))) = NoParse -- everything provided, no need to parse at all

-- Slightly neurotic? Nicer printing of the input seasons.
showFetchMessage :: [Int] -> ColorText
showFetchMessage seasons = case seasons of
  [s] -> base <> " " <> colorNum s -- e.g. "season 12"
  [f, s] -> base <> "s " <> colorNum f <> " and " <> colorNum s -- e.g. "seasons 12 and 13"
  threeOrMore -> base <> "s " <> withCommas threeOrMore -- e.g. "seasons 12, 13, 14"
  where
    base = "Fetching episode data from API for season"
    withCommas l = mconcat $ intersperse ", " $ map colorNum l

colorNum :: Int -> ColorText
colorNum = cyan . T.pack . show

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
    showMatchFailures failures = asWarning $ printRelativeFiles failures matchError

-- If the user isn't actually requiring us to parse out data, we don't care if
-- there are parsing errors.
--
-- TODO: This logic will likely have to be adjusted when we start parsing out
-- show names as well as season nums.
showParseFailures :: (MonadIO m) => SearchType -> ParseResults -> m ()
showParseFailures NoParse _ = return () -- No parsing means no failures
showParseFailures _ (ParseResults _ _ []) = return () -- No failures also means no failures!
showParseFailures _ (ParseResults _ _ failures) = asWarning $ printRelativeFiles failures errMsg
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
getSeasonNums :: (MonadError Error m) => Maybe Int -> ParseResults -> m [Int]
getSeasonNums (Just seasonNum) _ = return [seasonNum]
getSeasonNums Nothing parseResults = case seasonNumbers parseResults of
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
