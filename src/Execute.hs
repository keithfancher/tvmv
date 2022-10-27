module Execute
  ( TVMV,
    renameSeason,
    runTVMV,
  )
where

import API (APIKey, searchSeason)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.Text as T
import Error (Error)
import File (listDir)
import Rename (RenameOp, executeRename, renameFiles)
import Show (Episode, TvShow (..))

-- Wrap the stack!
type TVMV a = ExceptT Error IO a

runTVMV :: TVMV a -> IO (Either Error a)
runTVMV = runExceptT

-- Tie the pieces together, essentially.
renameSeason :: APIKey -> T.Text -> Int -> FilePath -> TVMV ()
renameSeason key name seasonNum directoryPath = do
  searchResults <- searchSeason key name seasonNum
  files <- liftIO $ listDir directoryPath
  renameOps <- liftEither $ renameWithData searchResults files
  liftIO $ executeRename renameOps

-- Convenience, pulling apart the tuple return and tying things back together.
renameWithData :: (TvShow, [Episode]) -> [FilePath] -> Either Error [RenameOp]
renameWithData (s, eps) = renameFiles (showName s) eps

-- Admittedly, I only vaguely understand why this works :')
liftEither :: Either Error a -> TVMV a
liftEither = ExceptT . return
