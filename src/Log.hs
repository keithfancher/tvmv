module Log
  ( readLogFile,
    readLatestLogFile,
    renameOpsFromText,
    writeLogFile,
    printAndWriteLog,
    printLog,
    getLogText,
    getLatestLog,
  )
where

import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf, sortBy)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Error (Error (..))
import Rename (RenameOp, RenameResult (..), printRenameResults)
import System.Directory (listDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Tvmv (Logger)

tvmvLogBaseFilename :: FilePath
tvmvLogBaseFilename = "tvmv-log"

-- Write (successful) results to a log file in the current directory.
writeLogFile :: Logger
writeLogFile [] = return () -- no ops, don't log
writeLogFile results = do
  logFile <- logFileName
  putStrLn $ "Writing log to `" <> logFile <> "`. Use `tvmv undo` to undo this operation."
  TIO.writeFile logFile (getLogText results)

-- Print only, don't write a log file.
printLog :: Logger
printLog [] = return () -- don't print if empty
printLog results = printRenameResults results

-- Print AND log. As you might expect.
printAndWriteLog :: Logger
printAndWriteLog results = printLog results >> writeLogFile results

-- Given a set of results, get the text to be written to a log file.
getLogText :: [RenameResult] -> T.Text
getLogText results = textify successes
  where
    -- Logfile is just `show`-ing the list of operations, we'll `read` it on
    -- the other end for `undo`:
    textify = T.pack . show
    successes = successfulOps results

successfulOps :: [RenameResult] -> [RenameOp]
successfulOps r = map op onlySuccesses
  where
    onlySuccesses = filter success r

-- Find the most recent tvmv log file in the CURRENT directory and attempt to
-- read it. If we don't find a log or if it's invalid, will return an Error.
readLatestLogFile :: (MonadIO m, MonadError Error m) => m [RenameOp]
readLatestLogFile = do
  files <- liftIO $ listDirectory "."
  case getLatestLog files of
    Nothing -> throwError $ UndoError "No tvmv log file found in current directory"
    Just latestLog -> readLogFile latestLog

-- Given a list of files, find the most recent tvmv log file, if there is one.
getLatestLog :: [FilePath] -> Maybe FilePath
getLatestLog paths = listToMaybe sortedLogFiles
  where
    sortedLogFiles = sortDesc $ filter isLogFile paths
    isLogFile = isInfixOf tvmvLogBaseFilename
    sortDesc = sortBy (flip compare) -- magic!

-- Attempt to read in a log file, which is actually valid Haskell, and parse
-- out the logged operations.
readLogFile :: (MonadIO m, MonadError Error m) => FilePath -> m [RenameOp]
readLogFile logFile = do
  liftIO $ putStrLn $ "Reading previously-completed operations from log file: " <> logFile
  text <- liftIO $ TIO.readFile logFile
  liftEither $ renameOpsFromText text

renameOpsFromText :: T.Text -> Either Error [RenameOp]
renameOpsFromText t = case readMaybe (T.unpack t) of
  Just results -> Right results
  Nothing -> Left $ UndoError "Invalid log file, unable to parse"

-- Use epoch timestamp as suffix. Just want it to be sorta-unique and sortable
-- by time.
logFileName :: IO FilePath
logFileName = printf pattern <$> epochTs
  where
    pattern = tvmvLogBaseFilename <> "-%d.txt"

epochTs :: IO Int
epochTs = round <$> getPOSIXTime
