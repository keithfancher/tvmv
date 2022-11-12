module Log
  ( readLogFile,
    renameOpsFromText,
    writeLogFile,
    printAndWriteLog,
    printLog,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error (Error (..))
import Rename (RenameOp, RenameResult (..), printRenameResult)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Tvmv (Logger)

writeLogFile :: Logger
writeLogFile [] = return [] -- no ops, don't log
writeLogFile results = do
  logFile <- logFileName
  let successes = successfulOps results
  TIO.writeFile logFile (textify successes) -- logging to a file in current directory
  return results
  where
    -- Logfile is just `show`-ing the list of operations, we'll `read` it on
    -- the other end for `undo`:
    textify = T.pack . show

printLog :: Logger
printLog [] = return [] -- don't print if empty
printLog results = do
  mapM_ printRenameResult results
  return results

printAndWriteLog :: Logger
printAndWriteLog results = printLog results >>= writeLogFile

successfulOps :: [RenameResult] -> [RenameOp]
successfulOps r = map op onlySuccesses
  where
    onlySuccesses = filter success r

-- Attempt to read in a log file, which is actually valid Haskell, and parse
-- out the logged operations.
readLogFile :: FilePath -> IO (Either Error [RenameOp])
readLogFile logFile = do
  text <- TIO.readFile logFile
  return $ renameOpsFromText text

renameOpsFromText :: T.Text -> Either Error [RenameOp]
renameOpsFromText t = case readMaybe (T.unpack t) of
  Just results -> Right results
  Nothing -> Left $ UndoError "Invalid log file, unable to parse"

-- Use epoch timestamp as suffix. Just want it to be sorta-unique and sortable
-- by time.
logFileName :: IO FilePath
logFileName = printf pattern <$> epochTs
  where
    pattern = "tvmv-log-%d.txt"

epochTs :: IO Int
epochTs = round <$> getPOSIXTime
