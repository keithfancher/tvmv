module Print.Pretty (Pretty (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Domain.Rename (RenameOp (..))
import Domain.Show (TvShow, showInfoBrief)
import GHC.IO.Exception (IOException (..))

class Pretty a where
  prettyText :: a -> T.Text -- only one required function
  prettyPrint :: (MonadIO m) => a -> m ()
  prettyPrintLn :: (MonadIO m) => a -> m ()
  prettyList :: [a] -> T.Text
  prettyPrintList :: (MonadIO m) => [a] -> m ()
  prettyPrintListLn :: (MonadIO m) => [a] -> m ()

  -- Default implementations:
  prettyPrint = liftIO . TIO.putStr . prettyText
  prettyPrintLn = liftIO . TIO.putStrLn . prettyText
  prettyList xs = T.intercalate defaultSeparator (map prettyText xs)
    where
      defaultSeparator = "\n\n"
  prettyPrintList = liftIO . TIO.putStr . prettyList
  prettyPrintListLn [] = return () -- don't want to print nothing and a newline
  prettyPrintListLn xs = liftIO . TIO.putStrLn . prettyList $ xs

instance Pretty TvShow where
  prettyText = showInfoBrief

instance Pretty RenameOp where
  prettyText renameOp = old <> " ->\n" <> new
    where
      old = T.pack $ oldPath renameOp
      new = T.pack $ newPath renameOp

-- Without unwrapping these exceptions ourselves, we get some very
-- user-unfriendly errors. Like:
--     user error (Destination file already exists: /home/ktf/tvmv-test-tmp/Absolutely Fabulous - s02e03 - Morocco.srt)
--     /home/ktf/tvmv-test-tmp/Absolutely Fabulous - s02e03.mp4: renameFile:renamePath:rename: permission denied (Permission denied)
--
-- Instead we want to see something like:
--     Destination file already exists: /home/ktf/tvmv-test-tmp/Absolutely Fabulous - s02e03 - Morocco.srt
--     Permission denied: /home/ktf/tvmv-test-tmp/Absolutely Fabulous - s02e03.mp4:
--
-- Because these are, to some extent, "expected" Exceptions, we don't need most
-- of the extra metadata, and the user certainly doesn't need it.
instance Pretty IOException where
  prettyText IOError {ioe_description = desc, ioe_filename = fn} = T.pack $ desc <> fileInfo fn
    where
      fileInfo (Just fp) = ": " <> fp
      fileInfo Nothing = ""
