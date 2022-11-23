module Exec.Rename
  ( executeRename,
    makeOpRelative,
    makeResultRelative,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Domain.Rename (RenameOp (..), RenameResult (..))
import System.Directory (makeRelativeToCurrentDirectory)
import qualified System.Directory as Dir

-- Actually rename the files on the file system. Accumulate a "log" of rename
-- ops.
executeRename :: (MonadIO m, MonadWriter [RenameResult] m) => [RenameOp] -> m ()
executeRename = mapM_ executeRenameSingle

-- Rename a single file on the file system. Assuming an IO Exception isn't
-- thrown, that op will be added to the Writer values for later logging.
-- TODO: Catch exceptions? Could also push failures into the Writer.
executeRenameSingle :: (MonadIO m, MonadWriter [RenameResult] m) => RenameOp -> m ()
executeRenameSingle (RenameOp old new) = do
  liftIO $ Dir.renameFile old new
  tell [RenameResult (RenameOp old new) True]

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
