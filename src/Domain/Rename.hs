module Domain.Rename
  ( RenameOp (..),
    MatchedEpisodes, -- note NOT exporting constructor(s) here
    matchEpisodes,
    matchEpisodesAllowPartial,
    renameFile,
    renameFiles,
    undoRenameOp,
  )
where

import qualified Data.Text as T
import Domain.Error (Error (..))
import Domain.Show (Episode (..))
import System.FilePath (replaceBaseName)
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
data MatchedEpisodes = MatchedEpisodes
  { episodes :: [Episode],
    files :: [FilePath]
  }
  deriving (Eq, Show)

-- "[show] - [season]x[ep] - [ep name]"
-- e.g. "Buffy the Vampire Slayer - 4x10 - Hush"
--
-- Note that this zero-pads the *episode* number but not the season, for
-- example: "Buffy the Vampire Slayer - 4x08 - Pangs"
--
-- Could make this a config option, or try to be smart based on total number of
-- seasons/episodes. But for now, this is a sane default.
episodeNameTemplate :: FilePath
episodeNameTemplate = "%s - %dx%02d - %s"

-- Strike that, reverse it!
undoRenameOp :: RenameOp -> RenameOp
undoRenameOp (RenameOp old new) = RenameOp {oldPath = new, newPath = old}

-- (Attempt to) tie together a set of files and Episode data. Fail if the lists
-- are different lengths.
matchEpisodes :: [Episode] -> [FilePath] -> Either Error MatchedEpisodes
matchEpisodes eps inFiles
  | length eps /= length inFiles = Left $ RenameError "Mismatched number of episodes and filenames"
  | otherwise = Right MatchedEpisodes {episodes = eps, files = inFiles}

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
  | length eps >= numFiles = Right $ MatchedEpisodes {episodes = partialEps, files = inFiles}
  | otherwise = Left $ RenameError "Mismatched number of episodes and filenames: too many files"
  where
    numFiles = length inFiles
    partialEps = take numFiles eps

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
    newFullPath = replaceBaseName inFile newBaseName

-- Generate the filename -- note, NOT the full path, nor the extension.
generateBaseFileName :: Episode -> FilePath
generateBaseFileName ep =
  printf
    episodeNameTemplate
    (T.unpack $ episodeShowName ep)
    (episodeSeasonNumber ep)
    (episodeNumber ep)
    (T.unpack $ episodeName ep)
