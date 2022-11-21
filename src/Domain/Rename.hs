module Domain.Rename
  ( RenameOp (..),
    RenameResult (..),
    renameFile,
    renameFiles,
    undoRenameOp,
    printRenameResults,
    printRenameOps,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

data RenameResult = RenameResult
  { op :: RenameOp,
    success :: Bool -- TODO: capture error messages too
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

printRenameResults :: [RenameResult] -> IO ()
printRenameResults = printList prettyRenameResult

-- TODO: make these relative to current dir when printing, probably
printRenameOps :: [RenameOp] -> IO ()
printRenameOps = printList prettyRenameOp

-- Print a list of things that can be turned to Text.
printList :: (a -> T.Text) -> [a] -> IO ()
printList toText rs = TIO.putStrLn (T.intercalate "\n\n" listAsText)
  where
    listAsText = map toText rs

prettyRenameResult :: RenameResult -> T.Text
prettyRenameResult r = prettyRenameOp (op r) <> "\n" <> result (success r)
  where
    result True = "Sucess!"
    result False = "ERROR :("

prettyRenameOp :: RenameOp -> T.Text
prettyRenameOp renameOp = old <> " ->\n" <> new
  where
    old = T.pack $ oldPath renameOp
    new = T.pack $ newPath renameOp

-- Strike that, reverse it!
undoRenameOp :: RenameOp -> RenameOp
undoRenameOp (RenameOp old new) = RenameOp {oldPath = new, newPath = old}

-- Given data for a list of episodes and a list of current FilePaths, generate
-- RenameOps for all the episodes. (Most common use-case here would be with a
-- season, though it technically could be any group of episodes.)
--
-- NOTE: The ORDER MATTERS here. The files must be in the same order as the
-- episode data, as that's how they're paired.
--
-- TODO: Allow partial matches? Probably useful. Opt-in?
renameFiles :: [Episode] -> [FilePath] -> Either Error [RenameOp]
renameFiles eps inFiles
  | length eps /= length inFiles = Left $ RenameError "Mismatched number of episodes and filenames"
  | otherwise = Right (map toRenameOp epsAndFiles)
  where
    epsAndFiles = zip eps inFiles
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
