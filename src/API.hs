module API
  ( APIKey,
    MonadAPI (..),
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
  )
where

import qualified API.TMDB as TMDB
import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Text as T
import Error (Error (..))
import Show (ItemId, Season (..), TvShow (..))
import Tvmv (Tvmv)

type APIKey = T.Text

-- Testing the API as a type class -- this might not work, as I'll potentially
-- want to swap APIs out without a whole other Monad stack...
class Monad m => MonadAPI m where
  getShow :: APIKey -> ItemId -> m TvShow
  getSeason :: APIKey -> TvShow -> Int -> m Season
  queryShows :: APIKey -> T.Text -> m [TvShow]

instance MonadAPI Tvmv where
  getShow = TMDB.getShow
  getSeason = TMDB.getSeason
  queryShows = TMDB.queryShows

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeasonByName :: (MonadAPI m, MonadError Error m) => APIKey -> T.Text -> Int -> m Season
searchSeasonByName apiKey query seasonNum = do
  showResults <- queryShows apiKey query
  firstResult <- liftEither $ firstMatch showResults
  getSeason apiKey firstResult seasonNum

-- Get season episode data using the show's unique ID directly. Doesn't
-- actually save on API calls, which is kind of funny, but ensures you're
-- getting exactly the match you want.
searchSeasonById :: MonadAPI m => APIKey -> ItemId -> Int -> m Season
searchSeasonById apiKey itemId seasonNum = do
  showData <- getShow apiKey itemId
  getSeason apiKey showData seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: MonadAPI m => APIKey -> T.Text -> m [TvShow]
searchShowByName = queryShows

firstMatch :: [a] -> Either Error a
firstMatch [] = Left $ APIError "No match found for search query"
firstMatch (first : _) = Right first
