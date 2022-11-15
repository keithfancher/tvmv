module Execute
  ( Env (..),
    execCommand,
    populateAPIKey,
  )
where

import API (APIKey, searchSeasonById, searchSeasonByName, searchShowByName)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error (..))
import File (listFiles)
import Log (printAndWriteLog, printLog, readLatestLogFile, readLogFile)
import Rename (executeRename, renameFiles, undoRenameOp)
import Show (Season (..), printShows)
import Text.Printf (printf)
import Tvmv (Tvmv, liftEither, mkTvmv, runTvmv)

-- Wrapper for other envinroment-related stuff we might need to execute a
-- command, in addition to the parsed-out CLI args.
data Env = Env
  { apiKeyEnvVar :: Maybe APIKey,
    apiKeyFile :: Maybe APIKey
  }

execCommand :: Env -> Command -> IO (Either Error ())
execCommand env (Mv mvOpts) = runWithLog $ renameSeason env mvOpts
execCommand env (Search searchOpts) = runNoLog $ searchByName env searchOpts
execCommand _ (Undo undoOpts) = runNoLog $ undoRename undoOpts

-- Run the Tvmv, print the results from its writer AND write a log file.
runWithLog :: Tvmv a -> IO (Either Error a)
runWithLog = runTvmv printAndWriteLog

-- Run the Tvmv, print the results ONLY. (No log file.)
runNoLog :: Tvmv a -> IO (Either Error a)
runNoLog = runTvmv printLog

-- Rename the files of a TV season.
renameSeason :: Env -> MvOptions -> Tvmv ()
renameSeason env (MvOptions maybeApiKey searchQuery seasNum inFiles) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  liftIO $ putStrLn "Fetching show data from API..."
  season <- searchSeason key seasNum
  files <- liftIO $ listFiles inFiles
  renameOps <- liftEither $ renameFiles (episodes season) files
  liftIO $ putStrLn $ renameMsg files
  lift $ executeRename renameOps
  where
    renameMsg f = printf "Renaming %d files..." (length f)
    searchSeason k = case searchQuery of
      (Name n) -> searchSeasonByName k n
      (Id i) -> searchSeasonById k i

-- Undo a previously-run rename operation, given a log file.
undoRename :: UndoOptions -> Tvmv ()
undoRename (UndoOptions maybeLogFileName) = do
  renameOps <- mkTvmv $ readLog maybeLogFileName
  let reversedOps = map undoRenameOp renameOps
  liftIO $ putStrLn $ undoMsg reversedOps
  lift $ executeRename reversedOps
  where
    undoMsg f = printf "Undoing rename of %d files..." (length f)
    readLog (Just fileName) = readLogFile fileName
    readLog Nothing = readLatestLogFile

-- Query the configured API for a show with the given name.
searchByName :: Env -> SearchOptions -> Tvmv ()
searchByName env (SearchOptions maybeApiKey searchQuery) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  liftIO $ putStrLn "Querying API..."
  tvShowResults <- searchShowByName key searchQuery
  liftIO $ putStrLn $ resultsMsg tvShowResults
  liftIO $ printShows tvShowResults
  where
    resultsMsg r = printf "Found %d results" (length r)

-- We check for API key in the following places, in the following order:
--
-- 1) CLI args. If it doesn't exist in CLI args, then check...
-- 2) The env var. If it doesn't exist in the env var, then check...
-- 3) The contents of a specific file. If it doesn't exist there, then...
--
-- ...return an error! This allows one to easily override the key in a
-- particular terminal session or even a particular call to `tvmv`. And gives
-- some flexibility to people who have different security needs/cares.
populateAPIKey :: Maybe APIKey -> Env -> Either Error APIKey
populateAPIKey (Just cliArgsKey) _ = Right cliArgsKey -- CLI args take precedence
populateAPIKey Nothing env = case envOrFile of
  Just k -> Right k
  Nothing -> Left missingKeyError
  where
    envOrFile = apiKeyEnvVar env <|> apiKeyFile env -- otherwise try these, in this order
    missingKeyError = InvalidInput "Missing API key! Please provide via command line args, env var, or file"
