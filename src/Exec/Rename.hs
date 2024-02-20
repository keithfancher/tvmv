module Exec.Rename
  ( RenameResult (..),
    executeRename,
    getOp,
    makeOpRelative,
    makeResultRelative,
    runRenameOps,
  )
where

import Control.Exception (try)
import Control.Monad.Except (MonadError, liftEither, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Text (Text)
import Domain.Error (Error (..))
import Domain.Rename (RenameOp (..))
import Print.Color (ColorText (..), Colorized (..))
import Print.Pretty (Pretty (..))
import System.Directory qualified as Dir

-- Not quite an Either, since we want the op to exist even in failure cases.
data RenameResult = Success RenameOp | Failure RenameOp IOError
  deriving (Eq, Show)

-- This lives here rather than with the other `Pretty` instances to avoid an
-- annoying circular dependency.
instance Pretty RenameResult where
  prettyText r = prettyText (getOp r) <> "\n" <> resultText r

instance Colorized RenameResult where
  colorize r = N opText <> resultColorText
    where
      opText = prettyText (getOp r) <> "\n"
      resultColorText = case r of
        (Success _) -> G $ resultText r
        (Failure _ _) -> R $ resultText r

resultText :: RenameResult -> Text
resultText (Success _) = "Rename successful!"
resultText (Failure _ err) = "ERROR :(\n  " <> prettyText err

-- Helper shared by `rename` and `undo` operations.
runRenameOps ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  [RenameOp] ->
  String ->
  Bool ->
  m ()
runRenameOps ops message forceRename = do
  putStrLn' message
  relativeOps <- mapM makeOpRelative ops -- we'll *print* relative paths, for readability
  prettyPrintListLn relativeOps >> putStrLn' ""
  awaitConfirmation forceRename
  putStrLn' "\nConfirmed! Renaming files...\n"
  executeRename ops

-- Given a `force` flag, either waits for the user to confirm an action, or
-- does nothing at all!
awaitConfirmation :: (MonadIO m, MonadError Error m) => Bool -> m ()
awaitConfirmation True = putStrLn' "`force` flag is set, proceeding...\n"
awaitConfirmation False = do
  putStrLn' "Continue? (y/N) " -- Note: need `putStrLn` here, not `putStr` (because buffering)
  input <- liftIO getChar
  liftEither $ confirm input
  where
    confirm 'y' = Right ()
    confirm 'Y' = Right ()
    confirm _ = Left UserAbort -- default is to bail

-- Wrapper for less lifting :')
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn

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

-- Attempt a single rename operation. Catches IO exceptions to return as an
-- `Either`. Will fail if destination path exists.
tryRename :: (MonadIO m) => FilePath -> FilePath -> m (Either IOError ())
tryRename old new = liftIO $ try $ do
  destFileExists <- Dir.doesPathExist new
  when destFileExists $ ioError fileExistsError
  Dir.renameFile old new
  where
    fileExistsError = userError $ "Destination file already exists: " <> new

mkResult :: RenameOp -> Either IOError () -> RenameResult
mkResult o (Left err) = Failure o err
mkResult o (Right _) = Success o

-- Replace the paths in a RenameOp with paths relative to the current directory.
makeOpRelative :: (MonadIO m) => RenameOp -> m RenameOp
makeOpRelative (RenameOp old new) = do
  relativeOld <- liftIO $ Dir.makeRelativeToCurrentDirectory old
  relativeNew <- liftIO $ Dir.makeRelativeToCurrentDirectory new
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
