module Execute (renameSeason) where

import API (APIKey, searchSeason)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Error (Error)
import File (listDir)
import Rename (RenameOp, executeRename, renameFiles)
import Show (Episode, TvShow (..))
import Tvmv (Tvmv, liftEither)

-- Tie the pieces together, essentially.
renameSeason :: APIKey -> T.Text -> Int -> FilePath -> Tvmv ()
renameSeason key name seasonNum directoryPath = do
  searchResults <- searchSeason key name seasonNum
  files <- liftIO $ listDir directoryPath
  renameOps <- liftEither $ renameWithData searchResults files
  liftIO $ executeRename renameOps

-- Convenience, pulling apart the tuple return and tying things back together.
renameWithData :: (TvShow, [Episode]) -> [FilePath] -> Either Error [RenameOp]
renameWithData (s, eps) = renameFiles (showName s) eps
