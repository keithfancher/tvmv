module Env (populateEnv) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Domain.API (APIKey)
import Execute (Env (..))
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

apiKeyEnvVarName :: String
apiKeyEnvVarName = "TMDB_API_KEY"

-- This will be relative to the config path, defined below
apiKeyFileName :: FilePath
apiKeyFileName = "tmdb-api-key"

populateEnv :: IO Env
populateEnv = do
  envVarContents <- lookupEnv apiKeyEnvVarName
  apiKeyFromFile <- apiKeyFileContents
  return $
    Env
      { apiKeyEnvVar = T.pack <$> envVarContents,
        apiKeyFile = apiKeyFromFile
      }

apiKeyFileContents :: IO (Maybe APIKey)
apiKeyFileContents = do
  configDir <- configDirPath
  let apiFileName = configDir </> apiKeyFileName
  fileExists <- doesFileExist apiFileName
  if fileExists
    then Just <$> getFileContents apiFileName
    else return Nothing
  where
    -- Note call to `strip` -- more than likely, the file will end with a newline:
    getFileContents fn = T.strip <$> TIO.readFile fn

-- On unix-ey systems this would be, e.g.: `~/.config/tvmv`
configDirPath :: IO FilePath
configDirPath = getXdgDirectory XdgConfig configDirName
  where
    configDirName = "tvmv"
