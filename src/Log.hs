module Log
  ( writeLogFileAndPrint,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Rename (executeRenameDryRun)
import Text.Printf (printf)
import Tvmv (Logger)

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
