module Print (Pretty (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Domain.Rename (RenameOp (..))
import Domain.Show (TvShow, showInfoBrief)

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
