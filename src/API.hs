module API
  ( APIKey,
    Error,
    searchShowByName,
  )
where

import qualified Data.Text as T
import qualified Network.API.TheMovieDB as TMDB
import Show (TvShow (..))

type APIKey = T.Text

type Error = T.Text

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIKey -> T.Text -> IO (Either Error [TvShow])
searchShowByName key query = do
  tmdbResults <- TMDB.runTheMovieDB (TMDB.defaultSettings key) (TMDB.searchTV query)
  return (mapEither tmdbResults)

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

-- Remove traces of the TMDB types for both success and error responses.
-- TODO: Map error type to an internal type as well, probably. For now, just
-- use Text. All we're doing from here is printing it anyway.
mapEither :: Either TMDB.Error [TMDB.TV] -> Either Error [TvShow]
mapEither (Right s) = Right (map mapTvShow s)
mapEither (Left e) = Left $ T.pack $ show e
