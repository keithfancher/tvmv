module File
  ( listCurrentDir,
    listDir,
    sortCaseInsensitive,
  )
where

import Data.List (sortBy)
import qualified Data.Text as T
import System.Directory (listDirectory, makeAbsolute)

-- Get a list of files in the current directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
listCurrentDir :: IO [FilePath]
listCurrentDir = listDir currentPath
  where
    currentPath = "."

-- Get a list of files in the given directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
-- TODO: filters? whitelist globs, filter out hidden, etc.?
-- TODO: actually, do we care whether the paths are absolute here? maybe unnecessary
listDir :: FilePath -> IO [FilePath]
listDir path = do
  files <- listDirectory path
  let sorted = sortCaseInsensitive files -- `listDirectory` returns in a (seemingly?) random order
  mapM makeAbsolute sorted -- `listDirectory` returns relative paths, we want absolute

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
