module Execute
  ( execCommand,
    run,
  )
where

import API (searchSeasonById, searchSeasonByName, searchShowByName)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error)
import File (InFiles (..), listFiles)
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
renameSeason (MvOptions apiKey searchQuery seasNum filesInput) = do
  season <- searchSeason seasNum
  files <- liftIO $ listFiles inFiles
  renameOps <- liftEither $ renameFiles (episodes season) files
  lift $ executeRename renameOps
  where
    inFiles = mkInFiles filesInput
    searchSeason = case searchQuery of
      (Name n) -> searchSeasonByName apiKey n
      (Id i) -> searchSeasonById apiKey i

undoRename :: UndoOptions -> Tvmv ()
undoRename (UndoOptions logFileName) = do
  renameOps <- mkTvmv $ readLogFile logFileName
  let reversedOps = map undoRenameOp renameOps
  lift $ executeRename reversedOps -- TODO: do I want the undo op to also generate a log?

searchByName :: SearchOptions -> Tvmv ()
searchByName (SearchOptions key searchQuery) = do
  showResults <- searchShowByName key searchQuery
  liftIO $ printShows showResults -- TODO: search shouldn't gen a log either...

-- Given the list of FilePaths -- which is how our input comes in from the CLI
-- arg parsing -- create the appropriate `InFiles` type.
-- TODO: This does NOT handle the case of passing in a single file. If there's
-- only one value, we're assuming it's a directory. (Also, test!)
mkInFiles :: [FilePath] -> InFiles
mkInFiles [] = Dir "." -- empty, default to current directory
mkInFiles [d] = Dir d -- one element passed in, assume it's a directory
mkInFiles twoOrMore = Files twoOrMore -- otherwise, a list of files
