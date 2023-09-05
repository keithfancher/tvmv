module Exec.Filter (filterFiles) where

import Control.Monad (filterM)
import Log (isLogFile)
import System.Directory (doesDirectoryExist)

-- Filter out any unwanted files or file types.
--
-- Stuff we know we don't want to include in our input list of files, such as:
-- 1. tvmv log files (if the user is running more than once in the same
--    directory, these are just noise and can break things)
-- 2. Directories! We don't want the user to have to care about running tvmv in
--    a folder with video files *and* directories. It should do the right thing.
filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM notLogOrDirectory
  where
    notLogOrDirectory f = do
      let notLog = notLogFile f
      notDir <- notDirectory f
      return $ notLog && notDir
    notLogFile = not . isLogFile
    notDirectory f = not <$> doesDirectoryExist f -- `doesDirectoryExist` is basically `isDirectory`.
