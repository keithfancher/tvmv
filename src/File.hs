module File
  ( InFiles (..),
    listFiles,
    mkInFiles,
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
-- only one value, we're assuming it's a directory. (Also, test!)
mkInFiles :: [FilePath] -> InFiles
mkInFiles [] = Dir "." -- empty, default to current directory
mkInFiles [d] = Dir d -- one element passed in, assume it's a directory
mkInFiles twoOrMore = Files twoOrMore -- otherwise, a list of files

-- Given a dir or files, get back a SORTED list of ABSOLUTE file paths.
listFiles :: InFiles -> IO [FilePath]
listFiles (Dir dirPath) = listDir dirPath
listFiles (Files fileList) = mapM makeAbsolute (sortCaseInsensitive fileList)

-- Get a list of files in the given directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
listDir :: FilePath -> IO [FilePath]
listDir dirPath = do
  files <- listDirectory dirPath
  let sorted = sortCaseInsensitive files -- `listDirectory` returns in a (seemingly?) random order
  let fullPath = map prependDirPath sorted -- `listDirectory` also ONLY returns files, not their paths
  mapM makeAbsolute fullPath -- now that we have a full path (possibly) relative to current dir, make absolute
  where
    prependDirPath p = dirPath </> p -- `dirPath` could be relative or absolute, we don't know/care

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
