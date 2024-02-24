module Exec.Commands.Undo (undo) where

import Command (UndoOptions (..))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer (MonadWriter)
import Data.Text (pack)
import Domain.Error (Error (..))
import Domain.Rename (undoRenameOp)
import Exec.Rename (RenameResult, runRenameOps)
import Log (readLatestLogFile, readLogFile)
import Print.Color (cyan)

-- Undo a previously-run rename operation, given a log file. The `undo` command.
undo ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  UndoOptions ->
  m ()
undo (UndoOptions forceRename maybeLogFileName) = do
  renameOps <- readLog maybeLogFileName
  let reversedOps = map undoRenameOp renameOps
  if null renameOps
    then liftIO $ putStrLn noOpsMessage
    else runRenameOps reversedOps (undoMsg reversedOps) forceRename
  where
    readLog (Just fileName) = readLogFile fileName
    readLog Nothing = readLatestLogFile
    undoMsg f =
      "Undoing will result in the following " <> colorNum (length f) <> " rename operations...\n"
    colorNum = cyan . pack . show
    noOpsMessage = "The specified log file did not contain any successful rename operations.\nNothing to undo!"
