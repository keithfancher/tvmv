module Execute
  ( execCommand,
    run,
  )
where

import API (APIKey, searchSeasonById, searchSeasonByName, searchShowByName)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error (APIError))
import File (listFiles)
import Log (readLogFile, writeLogFileAndPrint)
import Rename (executeRename, renameFiles, undoRenameOp)
import Show (Season (..), printShows)
import Tvmv (Tvmv, liftEither, mkTvmv, runTvmv)

execCommand :: Command -> Tvmv ()
execCommand (Mv mvOpts) = renameSeason mvOpts
execCommand (Search searchOpts) = searchByName searchOpts
execCommand (Undo undoOpts) = undoRename undoOpts

-- Run it with the configured logger
run :: Tvmv a -> IO (Either Error a)
run tvmv = runTvmv tvmv writeLogFileAndPrint

-- Tie the pieces together, essentially.
renameSeason :: MvOptions -> Tvmv ()
renameSeason (MvOptions maybeApiKey searchQuery seasNum inFiles) = do
  apiKey <- liftEither $ populateAPIKey maybeApiKey
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

searchByName :: SearchOptions -> Tvmv ()
searchByName (SearchOptions maybeApiKey searchQuery) = do
  apiKey <- liftEither $ populateAPIKey maybeApiKey
  showResults <- searchShowByName apiKey searchQuery
  liftIO $ printShows showResults -- TODO: search shouldn't gen a log either...

populateAPIKey :: Maybe APIKey -> Either Error APIKey
populateAPIKey (Just k) = Right k
populateAPIKey Nothing = Left $ APIError "Missing API key!" -- TODO: check env and file in this case
