module File
  ( InFiles (..),
    listDir,
    listFiles,
    mkInFiles,
    normalizeFileList,
    sortCaseInsensitive,
  )
where

import Data.List (sortBy)
import qualified Data.Text as T
import System.Directory (listDirectory, makeAbsolute)
import System.FilePath ((</>))

-- Our input file selection. Can either be a directory OR a list of files.
data InFiles = Dir FilePath | Files [FilePath]

-- Given the list of FilePaths -- which is how our input comes in from the CLI
-- arg parsing -- create the appropriate `InFiles` type.
-- TODO: This does NOT handle the case of passing in a single file. If there's
-- only one value, we're assuming it's a directory.
mkInFiles :: [FilePath] -> InFiles
mkInFiles [] = Dir "." -- empty, default to current directory
mkInFiles [d] = Dir d -- one element passed in, assume it's a directory
mkInFiles twoOrMore = Files twoOrMore -- otherwise, a list of files

-- Given a dir or files, get back a SORTED list of ABSOLUTE file paths.
listFiles :: InFiles -> IO [FilePath]
listFiles (Dir dirPath) = listDir dirPath
listFiles (Files fileList) = mapM makeAbsolute (normalize fileList)
  where
    normalize = normalizeFileList Nothing -- no base path here

-- Get a list of files in the given directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
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
