module Exec.Commands
  ( renameSeason,
    undoRename,
    searchByName,
  )
where

-- This module does all the hairy orchestration of IO stuff, tying all the
-- disparate pieces together to actually execute each `tvmv` command. This is a
-- good place to start to get a good high-level sense of the operations each
-- command performs. (Note that, thanks to the magic of `mtl`, these functions
-- are not coupled to any particular monad or monad transformer stack.)

import API (searchSeasonById, searchSeasonByName, searchShowByName)
import Command (MvOptions (..), SearchKey (..), SearchOptions (..), SeasonSelection (..), UndoOptions (..))
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Data.List (intercalate)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (MatchedEpisodes, RenameOp, episodes, matchEpisodes, matchEpisodesAllowPartial, renameFiles, undoRenameOp)
import Domain.Show (Episode, Season (..))
import Exec.Env (Env, populateAPIKey)
import Exec.Filter (filterFiles)
import Exec.Rename (RenameResult, executeRename, makeOpRelative)
import File (listFiles)
import Log (readLatestLogFile, readLogFile)
import Match (MatchResults (..), ParseResults (..), ParsedFile, matchParsedEpisodes, parseFilePaths)
import Print (prettyPrintListLn)
import System.Directory (makeRelativeToCurrentDirectory)
import Text.Printf (printf)

-- Rename the files of a TV season. This puts the `mv` in tvmv!
renameSeason ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  MvOptions ->
  m ()
renameSeason env withApi (MvOptions maybeApiKey force _ partialMatches searchQuery seasonSelection inFiles) = do
  apiKey <- liftEither $ populateAPIKey maybeApiKey env
  filteredFiles <- liftIO $ listFiles inFiles >>= filterFiles
  let parseResults = parseFilePaths filteredFiles
  showParseFailures seasonSelection parseResults
  seasonNum <- getSeasonNum seasonSelection parseResults
  putStrLn' $ "Fetching show data from API for season " <> show seasonNum <> "..."
  episodeData <- Domain.Show.episodes <$> searchSeason apiKey seasonNum
  matchedFiles <- case seasonSelection of
    SeasonNum _ -> liftEither $ match episodeData filteredFiles -- lexicographic sort, "dumb" matching
    Auto -> autoMatchFiles (successes parseResults) episodeData -- parsed filenames, "smart" matching
  let renameOps = renameFiles matchedFiles
  runRenameOps renameOps (renameMsg renameOps) force
  where
    match = if partialMatches then matchEpisodesAllowPartial else matchEpisodes
    renameMsg f = printf "Preparing to execute the following %d rename operations...\n" (length f)
    searchSeason k = case searchQuery of
      (Name n) -> searchSeasonByName withApi k n
      (Id i) -> searchSeasonById withApi k i

-- If there are any failed matches, we show them to the user but continue on.
-- If we get ZERO successful matches, however, the whole operation is a failure.
autoMatchFiles :: (MonadIO m, MonadError Error m) => [ParsedFile] -> [Episode] -> m MatchedEpisodes
autoMatchFiles parsedFiles epList = do
  let (MatchResults matchedEps f) = matchParsedEpisodes parsedFiles epList
  showMatchFailures f
  case Domain.Rename.episodes matchedEps of -- stupid ambiguous refs require this qualification :')
    [] -> throwError $ ParseError zeroMatchError
    _ -> return matchedEps
  where
    zeroMatchError =
      "No parsed files matched the API episode data ... try using the `-s` flag to manually specify the correct season?"
    matchError = "\nFailed to match the following files with corresponding API episode data:"
    showMatchFailures [] = return ()
    showMatchFailures failures = printRelativeFiles failures matchError

showParseFailures :: (MonadIO m) => SeasonSelection -> ParseResults -> m ()
showParseFailures (SeasonNum _) _ = return () -- No parsing
showParseFailures Auto (ParseResults _ _ []) = return () -- No failures!
showParseFailures Auto (ParseResults _ _ failures) = printRelativeFiles failures errMsg
  where
    errMsg = "Failed to parse season/episode numbers from the following files:"

-- A helper to get back a newline-delimted string of relative paths, given a
-- list of FilePaths. With a message at the top!
printRelativeFiles :: (MonadIO m) => [FilePath] -> String -> m ()
printRelativeFiles files msg = do
  putStrLn' msg
  f <- liftIO withNewLines
  putStrLn' $ f <> "\n"
  where
    withNewLines = intercalate "\n" <$> relativeFiles
    relativeFiles = mapM makeRelativeToCurrentDirectory files

-- If the user has specified a season, simply return that. In the case of
-- auto-detection, pull the season from the parsed input file list. For now, we
-- only support a single season. If the input files span multiple seasons,
-- fail. For now!
getSeasonNum :: (MonadError Error m) => SeasonSelection -> ParseResults -> m Int
getSeasonNum (SeasonNum n) _ = return n
getSeasonNum Auto parseResults = case seasonNumbers parseResults of
  [s] -> return s -- exactly one season is a success, return that season
  [] -> throwError $ ParseError "Unable to parse season/episode data from any input files"
  _ -> throwError $ ParseError "All input files must be from a single season"

-- Undo a previously-run rename operation, given a log file. The `undo` command.
undoRename ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  UndoOptions ->
  m ()
undoRename (UndoOptions forceRename maybeLogFileName) = do
  renameOps <- readLog maybeLogFileName
  let reversedOps = map undoRenameOp renameOps
  runRenameOps reversedOps (undoMsg reversedOps) forceRename
  where
    undoMsg f = printf "Undoing will result in the following %d rename operations...\n" (length f)
    readLog (Just fileName) = readLogFile fileName
    readLog Nothing = readLatestLogFile

-- Query the configured API for a show with the given name. The `search` command!
searchByName ::
  (MonadIO m, MonadError Error m) =>
  Env ->
  APIWrapper m ->
  SearchOptions ->
  m ()
searchByName env withApi (SearchOptions maybeApiKey searchQuery) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  putStrLn' "Querying API..."
  tvShowResults <- searchShowByName withApi key searchQuery
  putStrLn' $ resultsMsg tvShowResults
  prettyPrintListLn tvShowResults
  where
    resultsMsg r = printf "Found %d results" (length r)

-- Helper shared by `rename` and `undo` operations.
runRenameOps ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  [RenameOp] ->
  String ->
  Bool ->
  m ()
runRenameOps ops message forceRename = do
  putStrLn' message
  relativeOps <- mapM makeOpRelative ops -- we'll *print* relative paths, for readability
  prettyPrintListLn relativeOps >> putStrLn' ""
  awaitConfirmation forceRename
  executeRename ops

-- Given a `force` flag, either waits for the user to confirm an action, or
-- does nothing at all!
awaitConfirmation :: (MonadIO m, MonadError Error m) => Bool -> m ()
awaitConfirmation True = putStrLn' "`force` flag is set, proceeding...\n"
awaitConfirmation False = do
  putStrLn' "Continue? (y/N) " -- Note: need `putStrLn` here, not `putStr` (because buffering)
  input <- liftIO getChar
  liftEither $ confirm input
  where
    confirm 'y' = Right ()
    confirm 'Y' = Right ()
    confirm _ = Left UserAbort -- default is to bail

-- Wrapper for less lifting :')
putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn
