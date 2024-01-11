module File
  ( listDir,
    listFiles,
    sortCaseInsensitive,
  )
where

import Data.List (sortBy)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))

-- Given:
--   a single directory
--   OR a list of one or more files
--   OR an empty list...
-- get back a SORTED list of ABSOLUTE file paths. (If given an empty list,
-- returns files in current directory.
listFiles :: [FilePath] -> IO [FilePath]
listFiles [] = listDir "." -- no files/paths given, default to current directory
listFiles [singlePath] = do
  pathIsDirectory <- doesDirectoryExist singlePath
  if pathIsDirectory -- a single path could be a directory OR a single file
    then listDir singlePath
    else mapM normalizePath [singlePath] -- jump through some hoops to get a list back
listFiles multiplePaths = normalizePaths multiplePaths

-- Get a list of files in the given directory. Note that this returns a SORTED
-- list of ABSOLUTE paths.
listDir :: FilePath -> IO [FilePath]
listDir dirPath = do
  files <- listDirectory dirPath
  let fixedFiles = map prependDir files
  normalizePaths fixedFiles
  where
    -- `listDirectory` only returns the files themselves, not any portion of
    -- the path. For example, if you `listDirectory some/path/`, you'll get
    -- results like `["file.a", "file.b"]`, without the `some/path/` portion.
    -- So we need to stick that back on for the (relative) paths to be correct.
    prependDir p = dirPath </> p

-- For a single file, "normalization" is simply making its path absolute.
normalizePath :: FilePath -> IO FilePath
normalizePath = makeAbsolute

-- For a list of files, we make each one absolute AND do a case-insensitive
-- sort of the list. `listDirectory` returns files in a (seemingly?)
-- non-deterministic order, so this is required.
normalizePaths :: [FilePath] -> IO [FilePath]
normalizePaths paths = mapM normalizePath (sortCaseInsensitive paths)

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
