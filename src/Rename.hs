module Rename
  ( RenameData (..),
    generateBaseFileName,
  )
where

import qualified Data.Text as T
import Show (Episode (..))
import Text.Printf (printf)

data RenameData = RenameData
  { showName :: T.Text,
    episode :: Episode
  }

-- TODO: a new filepath alias, but with Text rather than String?

-- "[show] - [season]x[ep] - [ep name]"
-- e.g. "Buffy the Vampire Slayer - 4x10 - Hush"
-- TODO: Zero-padding? Config? Or better yet, be smart based on total number of eps/seasons.
episodeNameTemplate :: FilePath
episodeNameTemplate = "%s - %dx%d - %s"

-- Given full file path, get the "base" file and generate its new filename.
-- TODO: Maybe return a tuple of old and new? Or define a new type for that.
renameFile :: RenameData -> FilePath -> FilePath
renameFile epData inFile = ""

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
