module Execute
  ( renameSeason,
    run,
  )
where

import API (APIKey, searchSeason)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Error (Error)
import File (listDir)
import Rename (executeRename, executeRenameDryRun, renameFiles)
import Show (Season (..))
import Tvmv (Logger, Tvmv, liftEither, runTvmv)

-- Tie the pieces together, essentially.
renameSeason :: APIKey -> T.Text -> Int -> FilePath -> Tvmv ()
renameSeason key name seasonNum directoryPath = do
  season <- searchSeason key name seasonNum
  files <- liftIO $ listDir directoryPath
  renameOps <- liftEither $ renameFiles (episodes season) files
  lift $ executeRename renameOps

-- Run it with the configured logger
run :: Tvmv a -> IO (Either Error a)
run tvmv = runTvmv tvmv logRenameOps

-- TODO: Inject a particular Logger into the `run` function based on config
-- (dry-run, etc?). For now, "logging" is just printing.
logRenameOps :: Logger
logRenameOps = executeRenameDryRun
