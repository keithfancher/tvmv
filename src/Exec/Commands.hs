module Exec.Commands
  ( renameSeason,
    undoRename,
    searchByName,
  )
where

import API (APIWrapper, searchSeasonById, searchSeasonByName, searchShowByName)
import Command (MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Error (Error (..))
import Exec.Env (Env, populateAPIKey)
import File (listFiles)
import Log (readLatestLogFile, readLogFile)
import Rename (RenameOp, executeRename, printRenameOps, renameFiles, undoRenameOp)
import Show (Season (..), printShows)
import Text.Printf (printf)
import Tvmv (Tvmv, mkTvmv)

-- Rename the files of a TV season.
renameSeason :: Env -> APIWrapper Tvmv -> MvOptions -> Tvmv ()
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
undoRename :: UndoOptions -> Tvmv ()
undoRename (UndoOptions forceRename maybeLogFileName) = do
  renameOps <- mkTvmv $ readLog maybeLogFileName
  let reversedOps = map undoRenameOp renameOps
  runRenameOps reversedOps (undoMsg reversedOps) forceRename
  where
    undoMsg f = printf "Undoing will result in the following %d rename operations...\n" (length f)
    readLog (Just fileName) = readLogFile fileName
    readLog Nothing = readLatestLogFile

-- Query the configured API for a show with the given name.
searchByName :: Env -> APIWrapper Tvmv -> SearchOptions -> Tvmv ()
searchByName env withApi (SearchOptions maybeApiKey searchQuery) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  liftIO $ putStrLn "Querying API..."
  tvShowResults <- searchShowByName withApi key searchQuery
  liftIO $ putStrLn $ resultsMsg tvShowResults
  liftIO $ printShows tvShowResults
  where
    resultsMsg r = printf "Found %d results" (length r)

-- Helper shared by `rename` and `undo` operations.
runRenameOps :: [RenameOp] -> String -> Bool -> Tvmv ()
runRenameOps ops message forceRename = do
  liftIO $ putStrLn message
  liftIO $ printRenameOps ops >> putStrLn ""
  awaitConfirmation forceRename
  executeRename ops

-- Given a `force` flag, either waits for the user to confirm an action, or
-- does nothing at all!
awaitConfirmation :: Bool -> Tvmv ()
awaitConfirmation True = liftIO $ putStrLn "`force` flag is set, proceeding...\n"
awaitConfirmation False = do
  liftIO $ putStrLn "Continue? (y/N) " -- Note: need `putStrLn` here, not `putStr` (because buffering)
  input <- liftIO getChar
  liftEither $ confirm input
  where
    confirm 'y' = Right ()
    confirm 'Y' = Right ()
    confirm _ = Left UserAbort -- default is to bail
