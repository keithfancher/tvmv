module API
  ( APIKey,
    searchSeason,
    searchShowByName,
  )
where

import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.Text as T
import Error (Error (..))
import qualified Network.API.TheMovieDB as TMDB
import Show (Episode (..), TvShow (..))

type APIKey = T.Text

-- Wrap the stack!
type TVMV a = ExceptT Error IO a

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
--
-- TODO: probably new data type for this return structure... it's essentially
-- the same as `RenameData`, also
searchSeason :: APIKey -> T.Text -> Int -> TVMV (TvShow, [Episode])
searchSeason key query seasonNum = toTVMV key showAndEpisodes
  where
    showAndEpisodes = querySeason query seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIKey -> T.Text -> TVMV [TvShow]
searchShowByName key query = toTVMV key tmdbShows
  where
    tmdbShows = queryShows query

-- Map to our internal monad stack, converting errors along the way.
toTVMV :: APIKey -> TMDB.TheMovieDB a -> TVMV a
toTVMV key x = ExceptT mappedResult
  where
    mappedResult = mapError <$> tmdbIO
    tmdbIO = runTMDB key x

-- Map Eithers to our internal Error type.
mapError :: Either TMDB.Error a -> Either Error a
mapError (Right r) = Right r
mapError (Left e) = Left $ APIError $ show e

-- Run it! Requires an API key.
runTMDB :: APIKey -> TMDB.TheMovieDB a -> IO (Either TMDB.Error a)
runTMDB key = TMDB.runTheMovieDB (TMDB.defaultSettings key)

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

mapTvEpisode :: TMDB.Episode -> Episode
mapTvEpisode e =
  Episode
    { episodeNumber = TMDB.episodeNumber e,
      episodeName = TMDB.episodeName e,
      episodeSeasonNumber = TMDB.episodeSeasonNumber e
    }

-- Get a list of (our) `Episode` objects from a TMDB `Season`
getEpisodes :: TMDB.Season -> [Episode]
getEpisodes s = map mapTvEpisode (TMDB.seasonEpisodes s)

-- Given a query string (for show title), get FIRST match from TMDB for that
-- title, then query episode data for the given season.
--
-- Note that for TMDB, you need to request first the show, then the season in
-- separate requests. The show request returns a small subset of data.
querySeason :: T.Text -> Int -> TMDB.TheMovieDB (TvShow, [Episode])
querySeason query seasonNum = do
  showResults <- firstMatchForName query
  seasonData <- TMDB.fetchTVSeason (TMDB.tvID showResults) seasonNum
  return (mapTvShow showResults, getEpisodes seasonData)

-- Get a list of shows from TMDB that match the given query.
queryShows :: T.Text -> TMDB.TheMovieDB [TvShow]
queryShows query = do
  tvdbShows <- TMDB.searchTV query
  return $ map mapTvShow tvdbShows

-- Search given query for a show name, return data for the first show returned
-- by the API.
firstMatchForName :: T.Text -> TMDB.TheMovieDB TMDB.TV
firstMatchForName query = do
  results <- TMDB.searchTV query
  return (head results) -- TODO: not safe, of course
