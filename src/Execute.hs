module Execute
  ( execCommand,
    run,
  )
where

import API (searchSeason)
import Command (Command (..), MvOptions (..), SearchKey (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error (Error)
import File (listDir)
import Rename (executeRename, executeRenameDryRun, renameFiles)
import Show (Season (..))
import Text.Printf (printf)
import Tvmv (Logger, Tvmv, liftEither, runTvmv)

execCommand :: Command -> Tvmv ()
execCommand (Mv mvOpts) = renameSeason mvOpts
execCommand (Search _) = error "Implement me ;)" -- TODO: Other commands!
execCommand (Undo _) = error "Implement me ;)"

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

-- Run it with the configured logger
run :: Tvmv a -> IO (Either Error a)
run tvmv = runTvmv tvmv writeLogFileAndPrint

-- TODO: Inject a particular Logger into the `run` function based on config
-- (dry-run, etc?). For now, we're printing and writing a log file every time.
writeLogFileAndPrint :: Logger
writeLogFileAndPrint ops = do
  executeRenameDryRun ops -- print as well as...
  logFile <- logFileName
  TIO.writeFile logFile textOps -- logging to a file in current directory
  where
    -- Logfile is just `show`-ing the list of operations, we'll `read` it on
    -- the other end for `undo`:
    textOps = T.pack $ show ops

-- Use epoch timestamp as suffix. Just want it to be sorta-unique and sortable
-- by time.
logFileName :: IO FilePath
logFileName = printf pattern <$> epochTs
  where
    pattern = "tvmv-log-%d.txt"

epochTs :: IO Int
epochTs = round <$> getPOSIXTime
