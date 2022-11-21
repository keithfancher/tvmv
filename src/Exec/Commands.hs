module Exec.Commands
  ( renameSeason,
    undoRename,
    searchByName,
  )
where

import API (searchSeasonById, searchSeasonByName, searchShowByName)
import Command (MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (RenameOp, RenameResult, executeRename, printRenameOps, renameFiles, undoRenameOp)
import Domain.Show (Season (..), printShows)
import Exec.Env (Env, populateAPIKey)
import File (listFiles)
import Log (readLatestLogFile, readLogFile)
import Text.Printf (printf)

-- Rename the files of a TV season.
renameSeason ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  MvOptions ->
  m ()
renameSeason env withApi (MvOptions maybeApiKey forceRename _ searchQuery seasNum inFiles) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  liftIO $ putStrLn "Fetching show data from API..."
  season <- searchSeason key seasNum
  files <- liftIO $ listFiles inFiles
  renameOps <- liftEither $ renameFiles (episodes season) files
  runRenameOps renameOps (renameMsg renameOps) forceRename
  where
    renameMsg f = printf "Preparing to execute the following %d rename operations...\n" (length f)
    searchSeason k = case searchQuery of
      (Name n) -> searchSeasonByName withApi k n
      (Id i) -> searchSeasonById withApi k i

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
  liftIO $ putStrLn "Querying API..."
  tvShowResults <- searchShowByName withApi key searchQuery
  liftIO $ putStrLn $ resultsMsg tvShowResults
  liftIO $ printShows tvShowResults
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
  liftIO $ putStrLn message
  liftIO $ printRenameOps ops >> putStrLn ""
  awaitConfirmation forceRename
  executeRename ops

-- Given a `force` flag, either waits for the user to confirm an action, or
-- does nothing at all!
awaitConfirmation :: (MonadIO m, MonadError Error m) => Bool -> m ()
awaitConfirmation True = liftIO $ putStrLn "`force` flag is set, proceeding...\n"
awaitConfirmation False = do
  liftIO $ putStrLn "Continue? (y/N) " -- Note: need `putStrLn` here, not `putStr` (because buffering)
  input <- liftIO getChar
  liftEither $ confirm input
  where
    confirm 'y' = Right ()
    confirm 'Y' = Right ()
    confirm _ = Left UserAbort -- default is to bail
