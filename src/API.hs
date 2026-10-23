module API
  ( defaultAPI,
    resolveAPIConfig,
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
  )
where

import API.TMDB (tmdbApiWrapper, tmdbDefaultAPIKey)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Domain.API (APIConfig (..), APIKey, APIWrapper (..), LanguageCode)
import Domain.Error (Error (..))
import Domain.Show (ItemId, Season (..), TvShow (..))
import Exec.Env (Env, populateAPIKey, populateLanguage)
import Monad.Tvmv (Tvmv)

defaultAPI :: APIWrapper Tvmv
defaultAPI = tmdbApiWrapper

defaultAPIKey :: APIKey
defaultAPIKey = tmdbDefaultAPIKey

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeasonByName :: (MonadError Error m) => APIWrapper m -> APIConfig -> T.Text -> Int -> m Season
searchSeasonByName withApi apiConfig query seasonNum = do
  showResults <- queryShows withApi apiConfig query
  firstResult <- liftEither $ firstMatch showResults
  getSeason withApi apiConfig firstResult seasonNum

-- Get season episode data using the show's unique ID directly. Doesn't
-- actually save on API calls, which is kind of funny, but ensures you're
-- getting exactly the match you want.
searchSeasonById :: (Monad m) => APIWrapper m -> APIConfig -> ItemId -> Int -> m Season
searchSeasonById withApi apiConfig itemId seasonNum = do
  showData <- getShow withApi apiConfig itemId
  getSeason withApi apiConfig showData seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIWrapper m -> APIConfig -> T.Text -> m [TvShow]
searchShowByName = queryShows

firstMatch :: [a] -> Either Error a
firstMatch [] = Left $ APIError "No match found for search query"
firstMatch (first : _) = Right first

-- Everything we need before we can talk to the API: the key and the language.
-- Each can come from a cli arg, an environment var, or a file.
resolveAPIConfig :: (MonadIO m, MonadError Error m) => Maybe APIKey -> Maybe LanguageCode -> Env -> m APIConfig
resolveAPIConfig cliArgsKey cliArgsLang env = do
  key <- resolveAPIKey cliArgsKey env
  lang <- liftEither $ populateLanguage cliArgsLang env
  printLanguage lang
  return $ APIConfig key lang
  where
    printLanguage Nothing = return () -- API's default, nothing to report
    printLanguage (Just l) = liftIO $ putStrLn $ "Using language: " <> T.unpack l

-- Attempt to populate the user's API key override, if it exists (from cli arg,
-- environment var, or file). If user has not provided an API key, fall back to
-- tvmv's "default" key for this API.
resolveAPIKey :: (MonadIO m) => Maybe APIKey -> Env -> m APIKey
resolveAPIKey cliArgsKey env = case populateAPIKey cliArgsKey env of
  (Left _) -> do
    liftIO $ putStrLn "User API key override not detected, using tvmv's default API key"
    return defaultAPIKey
  (Right k) -> do
    liftIO $ putStrLn "Found user-provided API key override..."
    return k
