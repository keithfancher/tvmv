module Rename
  ( RenameOp (..),
    executeRename,
    executeRenameDryRun,
    renameFile,
    renameFiles,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Writer (WriterT, tell)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Error (Error (..))
import Show (Episode (..))
import qualified System.Directory as Dir
import System.FilePath (replaceBaseName)
import Text.Printf (printf)

-- Contains a rename "op" for a single file. Either to be performed or which
-- has been performed.
data RenameOp = RenameOp
  { oldPath :: FilePath,
    newPath :: FilePath
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

-- Actually rename the files. Accumulate a "log" of rename ops.
executeRename :: [RenameOp] -> WriterT [RenameOp] IO ()
executeRename = mapM_ executeRenameSingle

-- Rename a single file on the file system. Assuming an IO Exception isn't
-- thrown, that op will be added to the Writer values for later logging.
-- TODO: Catch exceptions? Could also push failures into the Writer, maybe
-- define a RenameResult type or something?
executeRenameSingle :: RenameOp -> WriterT [RenameOp] IO ()
executeRenameSingle (RenameOp old new) = do
  liftIO $ Dir.renameFile old new
  tell [RenameOp old new]

-- Just print what *would* have happened rather than actually renaming files.
executeRenameDryRun :: [RenameOp] -> IO ()
executeRenameDryRun = mapM_ printRenameOp

printRenameOp :: RenameOp -> IO ()
printRenameOp = TIO.putStrLn . prettyRenameOp

prettyRenameOp :: RenameOp -> T.Text
prettyRenameOp op = old <> " ->\n" <> new <> "\n---"
  where
    old = T.pack $ oldPath op
    new = T.pack $ newPath op

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
