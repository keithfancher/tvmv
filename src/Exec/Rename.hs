module Exec.Rename (executeRename) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Domain.Rename (RenameOp (..), RenameResult (..))
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
