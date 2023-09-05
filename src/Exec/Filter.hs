module Exec.Filter (filterFiles) where

import Log (isLogFile)

-- Filter out any unwanted files or file types.
--
-- Stuff we know we don't want to include in our input list of files, such as:
-- 1. tvmv log files (if the user is running more than once in the same
--    directory, these are just noise and can break things)
-- 2. Directories! We don't want the user to have to care about running tvmv in
--    a folder with video files *and* directories. It should do the right thing.
filterFiles :: [FilePath] -> IO [FilePath]
filterFiles inFiles = return $ filter notLogFile inFiles
  where
    notLogFile = not . isLogFile -- TODO: filter directories
