module Exec.Env
  ( Env (..),
    populateAPIKey,
  )
where

import Control.Applicative ((<|>))
import Domain.API (APIKey)
import Domain.Error (Error (..))

-- Wrapper for other envinroment-related stuff we might need to execute a
-- command, in addition to the parsed-out CLI args.
data Env = Env
  { apiKeyEnvVar :: Maybe APIKey,
    apiKeyFile :: Maybe APIKey
  }

-- We check for API key in the following places, in the following order:
--
-- 1) CLI args. If it doesn't exist in CLI args, then check...
-- 2) The env var. If it doesn't exist in the env var, then check...
-- 3) The contents of a specific file. If it doesn't exist there, then...
--
-- ...return an error! This allows one to easily override the key in a
-- particular terminal session or even a particular call to `tvmv`. And gives
-- some flexibility to people who have different security needs/cares.
populateAPIKey :: Maybe APIKey -> Env -> Either Error APIKey
populateAPIKey (Just cliArgsKey) _ = Right cliArgsKey -- CLI args take precedence
populateAPIKey Nothing env = case envOrFile of
  Just k -> Right k
  Nothing -> Left MissingAPIKey
  where
    envOrFile = apiKeyEnvVar env <|> apiKeyFile env -- otherwise try these, in this order
