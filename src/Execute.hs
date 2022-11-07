module Execute
  ( execCommand,
    run,
  )
where

import API (searchSeason, searchShowByName)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error)
import File (listDir)
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
renameSeason (MvOptions key name seasNum directoryPath) = do
  season <- searchSeason key (queryName name) seasNum
  files <- liftIO $ listDir directoryPath
  renameOps <- liftEither $ renameFiles (episodes season) files
  lift $ executeRename renameOps
  where
    queryName (Name n) = n
    queryName _ = error "No IDs yet ;)" -- TODO: Handle IDs in searches

undoRename :: UndoOptions -> Tvmv ()
undoRename (UndoOptions logFileName) = do
  renameOps <- mkTvmv $ readLogFile logFileName
  let reversedOps = map undoRenameOp renameOps
  lift $ executeRename reversedOps -- TODO: do I want the undo op to also generate a log?

searchByName :: SearchOptions -> Tvmv ()
searchByName (SearchOptions key searchQuery) = do
  showResults <- searchShowByName key searchQuery
  liftIO $ printShows showResults -- TODO: search shouldn't gen a log either...
