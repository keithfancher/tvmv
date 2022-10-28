module Execute (renameSeason) where

import API (APIKey, searchSeason)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import File (listDir)
import Rename (executeRename, renameFiles)
import Show (Season (..))
import Tvmv (Tvmv, liftEither)

-- Tie the pieces together, essentially.
renameSeason :: APIKey -> T.Text -> Int -> FilePath -> Tvmv ()
renameSeason key name seasonNum directoryPath = do
  season <- searchSeason key name seasonNum
  files <- liftIO $ listDir directoryPath
  renameOps <- liftEither $ renameFiles (episodes season) files
  liftIO $ executeRename renameOps
