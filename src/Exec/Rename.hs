module Exec.Rename
  ( RenameResult (..),
    executeRename,
    getOp,
    makeOpRelative,
    makeResultRelative,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Domain.Rename (RenameOp (..))
import System.Directory (makeRelativeToCurrentDirectory)
import System.Directory qualified as Dir

-- Not quite an Either, since we want the op to exist even in failure cases.
data RenameResult = Success RenameOp | Failure RenameOp IOError
  deriving (Eq, Show)

-- Actually rename the files on the file system. Accumulate a "log" of rename
-- results.
executeRename :: (MonadIO m, MonadWriter [RenameResult] m) => [RenameOp] -> m ()
executeRename = mapM_ executeRenameSingle

-- Rename a single file on the file system. The result of the operation,
-- whether a success or failure, will be added to the Writer values for later
-- logging.
executeRenameSingle :: (MonadIO m, MonadWriter [RenameResult] m) => RenameOp -> m ()
executeRenameSingle renameOp = do
  renameResults <- tryRename (oldPath renameOp) (newPath renameOp)
  tell [mkResult renameOp renameResults]

tryRename :: (MonadIO m) => FilePath -> FilePath -> m (Either IOError ())
tryRename old new = liftIO $ try (Dir.renameFile old new)

mkResult :: RenameOp -> Either IOError () -> RenameResult
mkResult o (Left err) = Failure o err
mkResult o (Right _) = Success o

-- Replace the paths in a RenameOp with paths relative to the current directory.
makeOpRelative :: (MonadIO m) => RenameOp -> m RenameOp
makeOpRelative (RenameOp old new) = do
  relativeOld <- liftIO $ makeRelativeToCurrentDirectory old
  relativeNew <- liftIO $ makeRelativeToCurrentDirectory new
  return RenameOp {oldPath = relativeOld, newPath = relativeNew}

makeResultRelative :: (MonadIO m) => RenameResult -> m RenameResult
makeResultRelative result = do
  relativeOp <- makeOpRelative (getOp result)
  return $ setOp result relativeOp

-- Gets op from a RenameResult.
getOp :: RenameResult -> RenameOp
getOp (Success op) = op
getOp (Failure op _) = op

-- Updates the op in a RenameResult.
setOp :: RenameResult -> RenameOp -> RenameResult
setOp (Success _) newOp = Success newOp
setOp (Failure _ err) newOp = Failure newOp err
