module Env (populateEnv) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Execute (Env (..))
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

apiKeyEnvVarName :: String
apiKeyEnvVarName = "TMDB_API_KEY"

languageEnvVarName :: String
languageEnvVarName = "TVMV_LANGUAGE"

-- These will be relative to the config path, defined below
apiKeyFileName :: FilePath
apiKeyFileName = "tmdb-api-key"

languageFileName :: FilePath
languageFileName = "language"

populateEnv :: IO Env
populateEnv = do
  apiKeyFromEnvVar <- lookupEnv apiKeyEnvVarName
  apiKeyFromFile <- configFileContents apiKeyFileName
  languageFromEnvVar <- lookupEnv languageEnvVarName
  languageFromFile <- configFileContents languageFileName
  return $
    Env
      { apiKeyEnvVar = T.pack <$> apiKeyFromEnvVar,
        apiKeyFile = apiKeyFromFile,
        languageEnvVar = T.pack <$> languageFromEnvVar,
        languageFile = languageFromFile
      }

-- The contents of the given file in our config directory, if it exists.
configFileContents :: FilePath -> IO (Maybe T.Text)
configFileContents fileName = do
  configDir <- configDirPath
  let fullPath = configDir </> fileName
  fileExists <- doesFileExist fullPath
  if fileExists
    then Just <$> getFileContents fullPath
    else return Nothing
  where
    -- Note call to `strip` -- more than likely, the file will end with a newline:
    getFileContents fn = T.strip <$> TIO.readFile fn

-- On unix-ey systems this would be, e.g.: `~/.config/tvmv`
configDirPath :: IO FilePath
configDirPath = getXdgDirectory XdgConfig configDirName
  where
    configDirName = "tvmv"
