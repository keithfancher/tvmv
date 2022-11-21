module API.Stub (testAPI) where

import API (APIKey, APIWrapper (..))
import qualified Data.Text as T
import Domain.Show (Episode (..), ItemId, Season (..), TvShow (..))
import Monad.Tvmv (Tvmv)

-- Just a simple stub! Hard-coded to return season 12 of Poirot for now, but
-- should be easy to flesh it out if we need more complex tests.
testAPI :: APIWrapper Tvmv
testAPI =
  APIWrapper
    { getShow = getShowStub,
      getSeason = getSeasonStub,
      queryShows = queryShowsStub
    }

getShowStub :: Monad m => APIKey -> ItemId -> m TvShow
getShowStub _ _ = return poirotShowData

getSeasonStub :: Monad m => APIKey -> TvShow -> Int -> m Season
getSeasonStub _ _ _ = return Season {seasonNumber = 12, episodes = poirotSeasonEps}

poirotSeasonEps :: [Episode]
poirotSeasonEps =
  [ Episode 1 "Three Act Tragedy" 12 poirotName,
    Episode 2 "Hallowe'en Party" 12 poirotName,
    Episode 3 "Murder on the Orient Express" 12 poirotName,
    Episode 4 "The Clocks" 12 poirotName
  ]

queryShowsStub :: Monad m => APIKey -> T.Text -> m [TvShow]
queryShowsStub _ _ = return [poirotShowData]

poirotName :: T.Text
poirotName = "Agatha Christie's Poirot"

poirotShowData :: TvShow
poirotShowData =
  TvShow
    { showId = 790,
      showName = poirotName,
      seasons = [],
      description = "!!!",
      numberOfSeasons = 0,
      numberOfEpisodes = 0
    }
