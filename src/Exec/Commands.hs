module Exec.Commands
  ( undoRename,
    searchByName,
  )
where

-- This module does all the hairy orchestration of IO stuff, tying all the
-- disparate pieces together to actually execute each `tvmv` command. This is a
-- good place to start to get a good high-level sense of the operations each
-- command performs. (Note that, thanks to the magic of `mtl`, these functions
-- are not coupled to any particular monad or monad transformer stack.)

import API (searchShowByName)
import Command (SearchOptions (..), UndoOptions (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Domain.Rename (RenameOp, undoRenameOp)
import Exec.Env (Env, populateAPIKey)
import Exec.Rename (RenameResult, executeRename, makeOpRelative)
import Log (readLatestLogFile, readLogFile)
import Print (prettyPrintListLn)
import Text.Printf (printf)

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
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn
