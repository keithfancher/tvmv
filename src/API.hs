module API
  ( defaultAPI,
    resolveAPIKey,
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
  )
where

import API.TMDB (tmdbApiWrapper, tmdbDefaultAPIKey)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Domain.API (APIKey, APIWrapper (..))
import Domain.Error (Error (..))
import Domain.Show (ItemId, Season (..), TvShow (..))
import Exec.Env (Env, populateAPIKey)
import Monad.Tvmv (Tvmv)

defaultAPI :: APIWrapper Tvmv
defaultAPI = tmdbApiWrapper

defaultAPIKey :: APIKey
defaultAPIKey = tmdbDefaultAPIKey

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeasonByName :: (MonadError Error m) => APIWrapper m -> APIKey -> T.Text -> Int -> m Season
searchSeasonByName withApi apiKey query seasonNum = do
  showResults <- queryShows withApi apiKey query
  firstResult <- liftEither $ firstMatch showResults
  getSeason withApi apiKey firstResult seasonNum

-- Get season episode data using the show's unique ID directly. Doesn't
-- actually save on API calls, which is kind of funny, but ensures you're
-- getting exactly the match you want.
searchSeasonById :: (Monad m) => APIWrapper m -> APIKey -> ItemId -> Int -> m Season
searchSeasonById withApi apiKey itemId seasonNum = do
  showData <- getShow withApi apiKey itemId
  getSeason withApi apiKey showData seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIWrapper m -> APIKey -> T.Text -> m [TvShow]
searchShowByName = queryShows

firstMatch :: [a] -> Either Error a
firstMatch [] = Left $ APIError "No match found for search query"
firstMatch (first : _) = Right first

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
