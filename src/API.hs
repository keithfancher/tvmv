module API
  ( APIKey,
    searchSeasonById,
    searchSeasonByName,
    searchShowByName,
  )
where

import qualified API.TMDB as API -- in theory, we can just swap this import to switch APIs
import qualified Data.Text as T
import Error (Error (..))
import Show (ItemId, Season (..), TvShow (..))
import Tvmv (Tvmv, liftEither)

type APIKey = T.Text

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeasonByName :: APIKey -> T.Text -> Int -> Tvmv Season
searchSeasonByName apiKey query seasonNum = do
  showResults <- API.queryShows apiKey query
  firstResult <- liftEither $ firstMatch showResults
  API.getSeason apiKey firstResult seasonNum

-- Get season episode data using the show's unique ID directly. Doesn't
-- actually save on API calls, which is kind of funny, but ensures you're
-- getting exactly the match you want.
searchSeasonById :: APIKey -> ItemId -> Int -> Tvmv Season
searchSeasonById apiKey itemId seasonNum = do
  showData <- API.getShow apiKey itemId
  API.getSeason apiKey showData seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIKey -> T.Text -> Tvmv [TvShow]
searchShowByName = API.queryShows

firstMatch :: [a] -> Either Error a
firstMatch [] = Left $ APIError "No match found!"
firstMatch (first : _) = Right first
