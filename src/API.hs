module API (searchShowByName) where

import qualified Data.Text as T
import qualified Network.API.TheMovieDB as TMDB
import Show (TvShow (..))

type APIKey = T.Text

-- Given a show name, or a fragment of a name, get back a list of matches.
-- TODO: Map error type to an internal type as well, probably.
searchShowByName :: APIKey -> T.Text -> IO (Either TMDB.Error [TvShow])
searchShowByName key query = do
  tmdbResults <- TMDB.runTheMovieDB (TMDB.defaultSettings key) (TMDB.searchTV query)
  return (mapShows <$> tmdbResults)
  where
    mapShows = map mapTvShow

-- Map from TMDB format to our internal domain. (Almost identical really, but
-- ours is a subset. Plus, mapping is always a sanity-saver.)
mapTvShow :: TMDB.TV -> TvShow
mapTvShow s =
  TvShow
    { showId = TMDB.tvID s,
      showName = TMDB.tvName s,
      seasons = [],
      description = TMDB.tvOverview s,
      numberOfSeasons = TMDB.tvNumberOfSeasons s,
      numberOfEpisodes = TMDB.tvNumberOfEpisodes s
    }
