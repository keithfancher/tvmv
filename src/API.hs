module API
  ( APIKey,
    APIWrapper (..),
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
    tmdbApiWrapper,
  )
where

import qualified API.TMDB as TMDB
import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Text as T
import Error (Error (..))
import Show (ItemId, Season (..), TvShow (..))
import Tvmv (Tvmv)

type APIKey = T.Text

data APIWrapper m = APIWrapper
  { getShow :: APIKey -> ItemId -> m TvShow,
    getSeason :: APIKey -> TvShow -> Int -> m Season,
    queryShows :: APIKey -> T.Text -> m [TvShow]
  }

tmdbApiWrapper :: APIWrapper Tvmv
tmdbApiWrapper =
  APIWrapper
    { getShow = TMDB.getShow,
      getSeason = TMDB.getSeason,
      queryShows = TMDB.queryShows
    }

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
