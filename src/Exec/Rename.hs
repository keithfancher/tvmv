module Exec.Rename
  ( RenameResult (..),
    executeRename,
    makeOpRelative,
    makeResultRelative,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Domain.Rename (RenameOp (..))
import System.Directory (makeRelativeToCurrentDirectory)
import qualified System.Directory as Dir

data RenameResult = RenameResult
  { op :: RenameOp,
    success :: Bool -- TODO: capture error messages too
  }
  deriving (Eq, Show)

-- Actually rename the files on the file system. Accumulate a "log" of rename
-- ops.
executeRename :: (MonadIO m, MonadWriter [RenameResult] m) => [RenameOp] -> m ()
executeRename = mapM_ executeRenameSingle

-- Rename a single file on the file system. Assuming an IO Exception isn't
-- thrown, that op will be added to the Writer values for later logging.
-- TODO: Catch exceptions? Could also push failures into the Writer.
executeRenameSingle :: (MonadIO m, MonadWriter [RenameResult] m) => RenameOp -> m ()
executeRenameSingle renameOp = do
  renameResults <- tryRename (oldPath renameOp) (newPath renameOp)
  tell [mkResult renameOp renameResults]

tryRename :: MonadIO m => FilePath -> FilePath -> m (Either IOError ())
tryRename old new = liftIO $ try (Dir.renameFile old new)

mkResult :: RenameOp -> Either IOError () -> RenameResult
mkResult o (Left err) = RenameResult {op = o, success = False}
mkResult o (Right _) = RenameResult {op = o, success = True}

-- Replace the paths in a RenameOp with paths relative to the current directory.
makeOpRelative :: MonadIO m => RenameOp -> m RenameOp
makeOpRelative (RenameOp old new) = do
  relativeOld <- liftIO $ makeRelativeToCurrentDirectory old
  relativeNew <- liftIO $ makeRelativeToCurrentDirectory new
  return RenameOp {oldPath = relativeOld, newPath = relativeNew}

makeResultRelative :: MonadIO m => RenameResult -> m RenameResult
makeResultRelative (RenameResult o s) = do
  relativeOp <- makeOpRelative o
  return RenameResult {op = relativeOp, success = s}
