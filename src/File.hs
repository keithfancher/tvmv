module File
  ( listDir,
    listFiles,
    normalizeFileList,
    sortCaseInsensitive,
  )
where

import Data.List (sortBy)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))

-- Given a directory OR a list of files OR nothing, get back a SORTED list of
-- ABSOLUTE file paths. If given an empty list, get files in current directory.
listFiles :: [FilePath] -> IO [FilePath]
listFiles [] = listDir "." -- no files/paths given, default to current directory
listFiles [singlePath] = do
  pathIsDir <- doesDirectoryExist singlePath
  if pathIsDir -- a single path could be a directory OR a single file
    then listDir singlePath
    else mapM makeAbsolute [singlePath]
listFiles multiplePaths = mapM makeAbsolute $ normalizeFileList Nothing multiplePaths

-- Get a list of files in the given directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
--
-- TODO: There's redundancy here, some weird organization, needs refactoring.
listDir :: FilePath -> IO [FilePath]
listDir dirPath = do
  files <- listDirectory dirPath
  let normalizedPaths = normalizeFileList (Just dirPath) files
  mapM makeAbsolute normalizedPaths

-- Pure helper func for the above. "Normalizing" here is:
--   1) `listDirectory` returns files in a (seemingly?) random order, so we sort.
--
--   2) `listDirectory` also only returns the files themselves, not any portion
--    of the path. For example, if you `listDirectory some/path/`, you'll get
--    results like `["file.a", "file.b"]`, without the `some/path/` portion. So
--    we need to stick that back on for the (relative) paths to be correct.
--
-- Note that the `basePath` is not required. This allows us to also use this
-- function even when given already "correct" relative paths, e.g., when a file
-- list is passed in directly rather than coming from `listDirectory`.
normalizeFileList :: Maybe FilePath -> [FilePath] -> [FilePath]
normalizeFileList basePath paths = map prependDirPath sorted
  where
    sorted = sortCaseInsensitive paths
    prependDirPath p = case basePath of
      Just base -> base </> p -- `basePath` could be relative or absolute, we don't know/care
      Nothing -> p

-- The default `sort` is case sensitive. For example:
--     sort ["test", "something", "Tesz", "Somethinz"]
-- yields:
--     ["Somethinz", "Tesz", "something", "test"]
-- This function is case insensitive, and would return:
--     ["something", "Somethinz", "test", "Tesz"]
sortCaseInsensitive :: [FilePath] -> [FilePath]
sortCaseInsensitive = sortBy caseInsensitiveCmp
  where
    caseInsensitiveCmp f1 f2 = compare (lower f1) (lower f2)
    lower = T.toLower . T.pack -- `Text.toLower` understands Unicode, so is preferred
