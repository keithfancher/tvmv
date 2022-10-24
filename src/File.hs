module File
  ( listCurrentDirectory,
  )
where

import Data.List (sort)
import System.Directory (listDirectory, makeAbsolute)

-- Get a list of files in the current directory. Note that this returns a
-- SORTED list of ABSOLUTE paths.
-- TODO: filters? whitelist globs, filter out hidden, etc.?
-- TODO: actually, do we care whether the paths are absolute here? maybe unnecessary
listCurrentDirectory :: IO [FilePath]
listCurrentDirectory = do
  files <- listDirectory currentDirectory
  let sorted = sort files -- `listDirectory` returns in a (seemingly?) random order
  mapM makeAbsolute sorted -- `listDirectory` returns relative paths, we want absolute
  where
    currentDirectory = "."
