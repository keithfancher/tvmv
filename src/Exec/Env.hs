module Exec.Env
  ( Env (..),
    populateAPIKey,
    populateLanguage,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAsciiUpper)
import Data.Set qualified as Set
import Data.Text qualified as T
import Domain.API (APIKey, LanguageCode)
import Domain.Error (Error (..))
import Language.Codes (iso639_1)

-- Wrapper for other envinroment-related stuff we might need to execute a
-- command, in addition to the parsed-out CLI args.
data Env = Env
  { apiKeyEnvVar :: Maybe APIKey,
    apiKeyFile :: Maybe APIKey,
    languageEnvVar :: Maybe LanguageCode,
    languageFile :: Maybe LanguageCode
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

-- The language is checked for in the same places, in the same order, as the
-- API key. Unlike the key, however, it's optional: if it's missing everywhere,
-- we return Nothing and get the API's default language (English).
--
-- Whichever language we find, it has to be valid. Better to fail with a
-- helpful message than to send the API something like "italian".
populateLanguage :: Maybe LanguageCode -> Env -> Either Error (Maybe LanguageCode)
populateLanguage cliArgsLang env = traverse validateLanguage firstFound
  where
    firstFound = cliArgsLang <|> languageEnvVar env <|> languageFile env

-- A two-letter ISO 639-1 language code, optionally followed by a two-letter
-- (uppercase) country code. For example: "it" or "it-IT".
validateLanguage :: LanguageCode -> Either Error LanguageCode
validateLanguage code = case T.splitOn "-" code of
  [lang] | isLanguage lang -> Right code
  [lang, country] | isLanguage lang && isCountry country -> Right code
  _invalid -> Left $ InvalidLanguage $ T.unpack code
  where
    isLanguage lang = T.unpack lang `Set.member` iso639_1
    isCountry country = T.length country == 2 && T.all isAsciiUpper country
