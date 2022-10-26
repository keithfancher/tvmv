module Execute (renameSeason) where

import API (APIKey, searchSeason)
import Control.Monad (join)
import qualified Data.Text as T
import File (listDir)
import Rename (RenameOp, executeRename, renameFiles)
import Show (Episode, TvShow (..))

-- Tie the pieces together, essentially.
renameSeason :: APIKey -> T.Text -> Int -> FilePath -> IO (Either T.Text ())
renameSeason key name seasonNum directoryPath = do
  searchResults <- searchSeason key name seasonNum
  files <- listDir directoryPath
  let renameOps = join $ renameWithData <$> searchResults <*> pure files
  mapM executeRename renameOps

-- Convenience, pulling apart the tuple return and tying things together.
renameWithData :: (TvShow, [Episode]) -> [FilePath] -> Either T.Text [RenameOp]
renameWithData (s, eps) = renameFiles (showName s) eps
