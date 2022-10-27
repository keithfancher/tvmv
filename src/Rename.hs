module Rename
  ( RenameData (..),
    RenameOp (..),
    executeRename,
    renameFile,
    renameFiles,
  )
where

import qualified Data.Text as T
import Error (Error (..))
import Show (Episode (..))
import System.FilePath (replaceBaseName)
import Text.Printf (printf)

-- All the pieces of data we need to correctly (re)name an episode file.
data RenameData = RenameData
  { showName :: T.Text,
    episode :: Episode
  }
  deriving (Eq, Show)

-- Contains a rename "op" for a single file. Either to be performed or which
-- has been performed.
data RenameOp = RenameOp
  { oldPath :: FilePath,
    newPath :: FilePath
  }
  deriving (Eq, Show)

-- "[show] - [season]x[ep] - [ep name]"
-- e.g. "Buffy the Vampire Slayer - 4x10 - Hush"
-- TODO: Zero-padding? Config? Or better yet, be smart based on total number of eps/seasons.
episodeNameTemplate :: FilePath
episodeNameTemplate = "%s - %dx%d - %s"

-- Actually rename the files. Accumulate a log file of rename ops.
-- TODO: just printing for now, testing!
executeRename :: [RenameOp] -> IO ()
executeRename = print

-- Given data for a list of episodes and a list of current FilePaths, generate
-- RenameOps for all the episodes. (Most common use-case here would be with a
-- season, though it technically could be any group of episodes.)
-- NOTE: The ORDER MATTERS here. The files must be in the same order as the
-- episode data, as that's how they're paired.
-- TODO: Allow partial matches? Probably useful. Opt-in?
renameFiles :: T.Text -> [Episode] -> [FilePath] -> Either Error [RenameOp]
renameFiles name eps inFiles
  | length eps /= length inFiles = Left $ RenameError "Mismatched number of episodes and filenames"
  | otherwise = Right (map toRenameOp epDataAndFiles)
  where
    epDataAndFiles = zip (map toRenameData eps) inFiles
    toRenameData ep = RenameData {showName = name, episode = ep}
    toRenameOp (d, p) = renameFile d p -- tuple to RenameOp object

-- Given a full file path and the episode metadata for that file, generate a
-- new filename.
renameFile :: RenameData -> FilePath -> RenameOp
renameFile epData inFile = RenameOp {oldPath = inFile, newPath = newFullPath}
  where
    newBaseName = generateBaseFileName epData
    newFullPath = replaceBaseName inFile newBaseName

-- Generate the filename -- note, NOT the full path, nor the extension.
generateBaseFileName :: RenameData -> FilePath
generateBaseFileName epData =
  printf
    episodeNameTemplate
    (T.unpack $ showName epData)
    (episodeSeasonNumber ep)
    (episodeNumber ep)
    (T.unpack $ episodeName ep)
  where
    ep = episode epData
