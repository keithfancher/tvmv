module Rename
  ( RenameData (..),
    RenameOp (..),
    renameFile,
  )
where

import qualified Data.Text as T
import Show (Episode (..))
import System.FilePath (replaceBaseName)
import Text.Printf (printf)

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

-- Attempt to guess/figure out the required data based on the input file alone.
-- TODO
deriveRenameData :: FilePath -> Maybe RenameData
deriveRenameData inFile = Nothing
