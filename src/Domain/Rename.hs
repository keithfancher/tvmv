module Domain.Rename
  ( RenameOp (..),
    MatchedEpisodes, -- note NOT exporting constructor(s) here
    episodes, -- but we ARE exporting field extractors
    files,
    matchEpisodes,
    matchEpisodes',
    matchEpisodesAllowPartial,
    renameFile,
    renameFiles,
    undoRenameOp,
  )
where

import Data.Text qualified as T
import Domain.Error (Error (..))
import Domain.Show (Episode (..))
import Filenames.Extensions (replaceBaseName')
import System.FilePath (makeValid)
import Text.Printf (printf)

-- Contains a rename "op" for a single file. Either to be performed or which
-- has been performed.
data RenameOp = RenameOp
  { oldPath :: FilePath,
    newPath :: FilePath
  }
  deriving (Eq, Show, Read)

-- A set of files and their associated Episode data. These two fields MUST be
-- the same length. Use the `matchEpisodes` function to construct this type.
data MatchedEpisodes = UnsafeMatchedEpisodes
  { episodes :: [Episode],
    files :: [FilePath]
  }
  deriving (Eq, Show)

-- A `printf` template for our episode filenames.
--
-- "[show name] - s[season#]e[ep#] - [ep name]"
-- e.g. "Buffy the Vampire Slayer - s04e10 - Hush"
--
-- Note that this zero-pads both the episode number and the season, for
-- example: "Buffy the Vampire Slayer - s04e08 - Pangs"
episodeNameTemplate :: FilePath
episodeNameTemplate = "%s - s%02de%02d - %s"

-- Strike that, reverse it!
undoRenameOp :: RenameOp -> RenameOp
undoRenameOp (RenameOp old new) = RenameOp {oldPath = new, newPath = old}

-- (Attempt to) tie together a set of files and Episode data. Fail if the lists
-- are different lengths.
matchEpisodes :: [Episode] -> [FilePath] -> Either Error MatchedEpisodes
matchEpisodes eps inFiles
  | numEps /= numFiles = Left $ RenameError errorMessage
  | otherwise = Right $ matchEpisodes' $ zip eps inFiles
  where
    numEps = length eps
    numFiles = length inFiles
    errorMessage = printf "Mismatched number of episodes (%d) and files (%d)" numEps numFiles

-- Same as above, but with a list of tuples as the input so invalid/unmatching
-- inputs are impossible!
matchEpisodes' :: [(Episode, FilePath)] -> MatchedEpisodes
matchEpisodes' inTuples = UnsafeMatchedEpisodes {episodes = eps, files = inFiles}
  where
    (eps, inFiles) = unzip inTuples

-- Allow partial matches. Creates a `MatchedEpisodes` object when given (a
-- certain class of) mismatched files/eps. Specifically, this is for the case
-- where (e.g.) the user only has the first "n" files of a season. Or
-- alternatively, if the API data has a bunch of extra stuff the user doesn't
-- have. (This is common with specials, e.g.) In other words, this allows the
-- case where there are more EPISODES than FILES.
--
-- Note that this function still disallows the inverse case: more files than
-- episodes. It doesn't really make sense... worst case, user can glob for the
-- file subset, put them in a directory, etc.
matchEpisodesAllowPartial :: [Episode] -> [FilePath] -> Either Error MatchedEpisodes
matchEpisodesAllowPartial eps inFiles
  | numEps >= numFiles = Right $ UnsafeMatchedEpisodes {episodes = partialEps, files = inFiles}
  | otherwise = Left $ RenameError errorMessage
  where
    numEps = length eps
    numFiles = length inFiles
    partialEps = take numFiles eps
    errorMessage = printf "Mismatched number of episodes (%d) and files (%d) -- too many files!" numEps numFiles

-- Given data for a list of episodes and a list of current FilePaths, generate
-- RenameOps for all the episodes. (Most common use-case here would be with a
-- season, though it technically could be any group of episodes.)
--
-- NOTE: The ORDER MATTERS here. The files must be in the same order as the
-- episode data, as that's how they're paired.
renameFiles :: MatchedEpisodes -> [RenameOp]
renameFiles matchedEps = map toRenameOp epsAndFiles
  where
    epsAndFiles = zip (episodes matchedEps) (files matchedEps)
    toRenameOp = uncurry renameFile -- uncurry? magic!

-- Given a full file path and the episode metadata for that file, generate a
-- new filename.
renameFile :: Episode -> FilePath -> RenameOp
renameFile ep inFile = RenameOp {oldPath = inFile, newPath = newFullPath}
  where
    newBaseName = generateBaseFileName ep
    -- Note final call to `makeValid`. Also note use of custom
    -- `replaceBaseName'`, which accounts for language metadata in filename.
    newFullPath = makeValid $ replaceBaseName' inFile newBaseName

-- Generate the filename -- note, NOT the full path, nor the extension. Note
-- the removal of delimiters to ensure no part of it is mistaken for a
-- directory.
generateBaseFileName :: Episode -> FilePath
generateBaseFileName ep =
  removeDelimiters $
    printf
      episodeNameTemplate
      (T.unpack $ episodeShowName ep)
      (episodeSeasonNumber ep)
      (episodeNumber ep)
      (T.unpack $ episodeName ep)

-- Technically a filename is "valid" even if it contains path delimiters (at
-- least according to `System.FilePath`, since it just assumes it's a full
-- path). That doesn't work for our case though, e.g. if the title of an
-- episode is `Stuff/things that Buffy does`, we don't want it to create a
-- `Stuff` directory. Also apparently newlines can exist in a filename?!
removeDelimiters :: FilePath -> FilePath
removeDelimiters = map replaceSlashes
  where
    replaceSlashes '/' = '-'
    replaceSlashes '\\' = '-'
    replaceSlashes '\n' = ' '
    replaceSlashes '\r' = ' '
    replaceSlashes nonSlash = nonSlash
