module Log
  ( readLogFile,
    writeLogFileAndPrint,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error (Error (..))
import Rename (RenameOp, executeRenameDryRun)
import Text.Printf (printf)
import Text.Read (readMaybe)
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

-- Attempt to read in a log file, which is actually valid Haskell, and parse
-- out the logged operations.
readLogFile :: FilePath -> IO (Either Error [RenameOp])
readLogFile logFile = do
  text <- TIO.readFile logFile
  return $ renameOpsFromText text

renameOpsFromText :: T.Text -> Either Error [RenameOp]
renameOpsFromText t = case readMaybe (T.unpack t) of
  Just ops -> Right ops
  Nothing -> Left $ UndoError "Invalid log file, unable to parse"

-- Use epoch timestamp as suffix. Just want it to be sorta-unique and sortable
-- by time.
logFileName :: IO FilePath
logFileName = printf pattern <$> epochTs
  where
    pattern = "tvmv-log-%d.txt"

epochTs :: IO Int
epochTs = round <$> getPOSIXTime
