module API
  ( defaultAPI,
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
  )
where

import API.TMDB (tmdbApiWrapper)
import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Text as T
import Domain.API (APIKey, APIWrapper (..))
import Domain.Error (Error (..))
import Domain.Show (ItemId, Season (..), TvShow (..))
import Monad.Tvmv (Tvmv)

defaultAPI :: APIWrapper Tvmv
defaultAPI = tmdbApiWrapper

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
searchSeasonById :: Monad m => APIWrapper m -> APIKey -> ItemId -> Int -> m Season
searchSeasonById withApi apiKey itemId seasonNum = do
  showData <- getShow withApi apiKey itemId
  getSeason withApi apiKey showData seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIWrapper m -> APIKey -> T.Text -> m [TvShow]
searchShowByName = queryShows

firstMatch :: [a] -> Either Error a
firstMatch [] = Left $ APIError "No match found for search query"
firstMatch (first : _) = Right first
