module Execute
  ( Env (..),
    execCommand,
    run,
  )
where

import API (APIKey, searchSeasonById, searchSeasonByName, searchShowByName)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error (..))
import File (listFiles)
import Log (readLogFile, writeLogFileAndPrint)
import Rename (executeRename, renameFiles, undoRenameOp)
import Show (Season (..), printShows)
import Tvmv (Tvmv, liftEither, mkTvmv, runTvmv)

-- Wrapper for other envinroment-related stuff we might need to execute a
-- command, in addition to the parsed-out CLI args.
data Env = Env
  { apiKeyEnvVar :: Maybe APIKey,
    apiKeyFile :: Maybe APIKey
  }

execCommand :: Env -> Command -> Tvmv ()
execCommand e (Mv mvOpts) = renameSeason e mvOpts
execCommand e (Search searchOpts) = searchByName e searchOpts
execCommand _ (Undo undoOpts) = undoRename undoOpts

-- Run it with the configured logger
run :: Tvmv a -> IO (Either Error a)
run tvmv = runTvmv tvmv writeLogFileAndPrint

-- Tie the pieces together, essentially.
renameSeason :: Env -> MvOptions -> Tvmv ()
renameSeason env (MvOptions maybeApiKey searchQuery seasNum inFiles) = do
  apiKey <- liftEither $ populateAPIKey maybeApiKey env
  season <- searchSeason apiKey seasNum
  files <- liftIO $ listFiles inFiles
  renameOps <- liftEither $ renameFiles (episodes season) files
  lift $ executeRename renameOps
  where
    searchSeason k = case searchQuery of
      (Name n) -> searchSeasonByName k n
      (Id i) -> searchSeasonById k i

undoRename :: UndoOptions -> Tvmv ()
undoRename (UndoOptions logFileName) = do
  renameOps <- mkTvmv $ readLogFile logFileName
  let reversedOps = map undoRenameOp renameOps
  lift $ executeRename reversedOps -- TODO: do I want the undo op to also generate a log?

searchByName :: Env -> SearchOptions -> Tvmv ()
searchByName env (SearchOptions maybeApiKey searchQuery) = do
  apiKey <- liftEither $ populateAPIKey maybeApiKey env
  showResults <- searchShowByName apiKey searchQuery
  liftIO $ printShows showResults -- TODO: search shouldn't gen a log either...

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
