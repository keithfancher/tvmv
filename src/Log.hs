module Log
  ( readLogFile,
    readLatestLogFile,
    renameOpsFromText,
    writeLogFile,
    printAndWriteLog,
    printLog,
    getLogText,
    getLatestLog,
    isLogFile,
  )
where

import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isPrefixOf, sortBy)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Domain.Error (Error (..))
import Domain.Rename (RenameOp)
import Exec.Rename (RenameResult (..), getOp, makeResultRelative)
import Monad.Tvmv (Logger)
import Print (prettyPrintListLn, printError, printSuccess, printWarning)
import System.Directory (listDirectory)
import System.FilePath (takeFileName)
import Text.Printf (printf)
import Text.Read (readMaybe)

tvmvLogBaseFilename :: FilePath
tvmvLogBaseFilename = "tvmv-log"

-- Write (successful) results to a log file in the current directory.
writeLogFile :: Logger
writeLogFile [] = return () -- no ops, don't log
writeLogFile results = do
  logFile <- logFileName
  putStrLn $ "Writing log to `" <> logFile <> "`.\nUse `tvmv undo` to undo any changes made by this operation."
  TIO.writeFile logFile (getLogText results)

-- Print only, don't write a log file.
printLog :: Logger
printLog [] = return () -- don't print if empty
printLog results = do
  relativeResults <- mapM makeResultRelative results -- print relative paths for readability
  prettyPrintListLn relativeResults
  colorPrint $ "\n" <> finalCountMsg
  where
    totalOps = length results
    numSuccesses = length $ successfulOps results
    numFailures = totalOps - numSuccesses
    finalCountMsg = show numSuccesses <> "/" <> show totalOps <> " operations succeeded. " <> show numFailures <> " failures.\n"
    colorPrint = case (numSuccesses, totalOps) of
      (suc, tot) | suc == tot -> printSuccess
      (0, _) -> printError
      _ -> printWarning

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
successfulOps r = map getOp onlySuccesses
  where
    onlySuccesses = filter isSuccess r
    isSuccess (Success _) = True
    isSuccess (Failure _ _) = False

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
    sortDesc = sortBy (flip compare) -- magic!

-- Is a given filename a tvmv log file?
isLogFile :: FilePath -> Bool
isLogFile f = tvmvLogBaseFilename `isPrefixOf` fileWithoutPath
  where
    fileWithoutPath = takeFileName f

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
