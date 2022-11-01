module API
  ( APIKey,
    searchSeason,
    searchShowByName,
  )
where

import qualified Data.Text as T
import Error (Error (..))
import qualified Network.API.TheMovieDB as TMDB
import Show (Episode (..), Season (..), TvShow (..))
import Tvmv (Tvmv, mkTvmv)

type APIKey = T.Text

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeason :: APIKey -> T.Text -> Int -> Tvmv Season
searchSeason key query seasonNum = toTvmv key $ querySeason query seasonNum

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIKey -> T.Text -> Tvmv [TvShow]
searchShowByName key query = toTvmv key tmdbShows
  where
    tmdbShows = queryShows query

-- Map to our internal monad stack, converting errors along the way.
toTvmv :: APIKey -> TMDB.TheMovieDB a -> Tvmv a
toTvmv key x = mkTvmv mappedResult
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
      seasons = map mapSeason (TMDB.tvSeasons s),
      description = TMDB.tvOverview s,
      numberOfSeasons = TMDB.tvNumberOfSeasons s,
      numberOfEpisodes = TMDB.tvNumberOfEpisodes s
    }
  where
    mapSeason = mapTvSeason (TMDB.tvName s) -- map with the given name

mapTvSeason :: T.Text -> TMDB.Season -> Season
mapTvSeason n s =
  Season
    { seasonNumber = TMDB.seasonNumber s,
      episodes = map mapEps (TMDB.seasonEpisodes s)
    }
  where
    mapEps = mapTvEpisode n -- map with the given name

mapTvEpisode :: T.Text -> TMDB.Episode -> Episode
mapTvEpisode n e =
  Episode
    { episodeNumber = TMDB.episodeNumber e,
      episodeName = TMDB.episodeName e,
      episodeSeasonNumber = TMDB.episodeSeasonNumber e,
      episodeShowName = n
    }

-- Given a query string (for show title), get FIRST match from TMDB for that
-- title, then query episode data for the given season.
--
-- Note that for TMDB, you need to request first the show, then the season in
-- separate requests. The show request returns a small subset of data.
querySeason :: T.Text -> Int -> TMDB.TheMovieDB Season
querySeason query seasonNum = do
  showResults <- firstMatchForName query
  seasonData <- TMDB.fetchTVSeason (TMDB.tvID showResults) seasonNum
  let name = TMDB.tvName showResults -- All we really need from the show here
  return $ mapTvSeason name seasonData

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
