module Execute
  ( execCommand,
    run,
  )
where

import API (searchSeason)
import Command (Command (..), MvOptions (..), SearchKey (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Error (Error)
import File (listDir)
import Log (writeLogFileAndPrint)
import Rename (executeRename, renameFiles)
import Show (Season (..))
import Tvmv (Tvmv, liftEither, runTvmv)

execCommand :: Command -> Tvmv ()
execCommand (Mv mvOpts) = renameSeason mvOpts
execCommand (Search _) = error "Implement me ;)" -- TODO: Other commands!
execCommand (Undo _) = error "Implement me ;)"

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
