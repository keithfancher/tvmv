module Env (populateEnv) where

import qualified Data.Text as T
import Execute (Env (..))
import System.Environment (lookupEnv)

apiKeyEnvVarName :: String
apiKeyEnvVarName = "TMDB_API_KEY"

populateEnv :: IO Env
populateEnv = do
  envVar <- lookupEnv apiKeyEnvVarName
  return $ Env {apiKeyEnvVar = T.pack <$> envVar, apiKeyFile = Nothing} -- TODO: file!
