module Exec.Commands.Undo (undo) where

import Command (UndoOptions (..))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer (MonadWriter)
import Domain.Error (Error (..))
import Domain.Rename (undoRenameOp)
import Exec.Rename (RenameResult, runRenameOps)
import Log (readLatestLogFile, readLogFile)
import Text.Printf (printf)

-- Undo a previously-run rename operation, given a log file. The `undo` command.
undo ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  UndoOptions ->
  m ()
undo (UndoOptions forceRename maybeLogFileName) = do
  renameOps <- readLog maybeLogFileName
  let reversedOps = map undoRenameOp renameOps
  runRenameOps reversedOps (undoMsg reversedOps) forceRename
  where
    undoMsg f = printf "Undoing will result in the following %d rename operations...\n" (length f)
    readLog (Just fileName) = readLogFile fileName
    readLog Nothing = readLatestLogFile
