module Print (Pretty (..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Domain.Rename (RenameOp (..), RenameResult (..))
import Domain.Show (TvShow, showInfoBrief)

class Pretty a where
  prettyText :: a -> T.Text -- only one required function
  prettyPrint :: a -> IO ()
  prettyPrintLn :: a -> IO ()
  prettyList :: [a] -> T.Text
  prettyPrintList :: [a] -> IO ()
  prettyPrintListLn :: [a] -> IO ()

  -- Default implementations:
  prettyPrint = TIO.putStr . prettyText
  prettyPrintLn = TIO.putStrLn . prettyText
  prettyList xs = T.intercalate defaultSeparator (map prettyText xs)
    where
      defaultSeparator = "\n\n"
  prettyPrintList = TIO.putStr . prettyList
  prettyPrintListLn [] = return () -- don't want to print nothing and a newline
  prettyPrintListLn xs = TIO.putStrLn . prettyList $ xs

instance Pretty TvShow where
  prettyText = showInfoBrief

instance Pretty RenameOp where
  prettyText renameOp = old <> " ->\n" <> new
    where
      old = T.pack $ oldPath renameOp
      new = T.pack $ newPath renameOp

instance Pretty RenameResult where
  prettyText r = prettyText (op r) <> "\n" <> result (success r)
    where
      result True = "Sucess!"
      result False = "ERROR :("
