module Exec.Commands
  ( renameSeason,
    undoRename,
    searchByName,
  )
where

import API (searchSeasonById, searchSeasonByName, searchShowByName)
import Command (MvOptions (..), SearchKey (..), SearchOptions (..), SeasonSelection (..), UndoOptions (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (RenameOp, matchEpisodes, matchEpisodesAllowPartial, renameFiles, undoRenameOp)
import Domain.Show (Season (..))
import Exec.Env (Env, populateAPIKey)
import Exec.Filter (filterFiles)
import Exec.Rename (RenameResult, executeRename, makeOpRelative)
import File (listFiles)
import Log (readLatestLogFile, readLogFile)
import Print (prettyPrintListLn)
import Text.Printf (printf)

-- Rename the files of a TV season.
renameSeason ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  MvOptions ->
  m ()
renameSeason env withApi (MvOptions maybeApiKey forceRename _ partialMatches searchQuery seasNum inFiles) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  putStrLn' "Fetching show data from API..."
  season <- searchSeason key $ getSeasonNum seasNum
  files <- liftIO $ listFiles inFiles
  filteredFiles <- liftIO $ filterFiles files
  matchedFiles <- liftEither $ match (episodes season) filteredFiles
  let renameOps = renameFiles matchedFiles
  runRenameOps renameOps (renameMsg renameOps) forceRename
  where
    match = if partialMatches then matchEpisodesAllowPartial else matchEpisodes
    renameMsg f = printf "Preparing to execute the following %d rename operations...\n" (length f)
    searchSeason k = case searchQuery of
      (Name n) -> searchSeasonByName withApi k n
      (Id i) -> searchSeasonById withApi k i
    -- TODO! A temporary shim, of course
    getSeasonNum (SeasonNum n) = n
    getSeasonNum Auto = undefined

-- Undo a previously-run rename operation, given a log file.
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

-- Query the configured API for a show with the given name.
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
