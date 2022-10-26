module API
  ( APIKey,
    APIError,
    searchSeason,
    searchShowByName,
  )
where

import qualified Data.Text as T
import qualified Network.API.TheMovieDB as TMDB
import Show (Episode (..), TvShow (..))

type APIKey = T.Text

type APIError = T.Text

-- Note that for TMDB, you need to request the SHOW, then the SEASON in
-- separate requests.
--
-- The TMDB bindings here have a convenience method to fech ALL the data for a
-- show. This will execute (I think?) an HTTP request for EACH season. So watch
-- out, might not be necessary for all use cases. Probably figure out some
-- caching here...

-- Search for a show with the given name and, using the FIRST match for that
-- name, return the episode data for the given season.
searchSeason :: APIKey -> T.Text -> Int -> IO (Either APIError (TvShow, [Episode]))
searchSeason key query seasonNum = do
  r <- TMDB.runTheMovieDB (TMDB.defaultSettings key) showAndEpisodes
  return (mapError r) -- convert errors to our domain
  where
    showAndEpisodes = mapTup <$> querySeason query seasonNum
    mapTup (sh, se) = (mapTvShow sh, getEpisodes se)

-- Given a show name, or a fragment of a name, get back a list of matches.
searchShowByName :: APIKey -> T.Text -> IO (Either APIError [TvShow])
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

mapTvEpisode :: TMDB.Episode -> Episode
mapTvEpisode e =
  Episode
    { episodeNumber = TMDB.episodeNumber e,
      episodeName = TMDB.episodeName e,
      episodeSeasonNumber = TMDB.episodeSeasonNumber e
    }

getEpisodes :: TMDB.Season -> [Episode]
getEpisodes s = map mapTvEpisode (TMDB.seasonEpisodes s)

-- Given a query string (for show title), get FIRST match from TMDB for that
-- title, then query episode data for the given season.
querySeason :: T.Text -> Int -> TMDB.TheMovieDB (TMDB.TV, TMDB.Season)
querySeason query seasonNum = do
  showResults <- firstMatchForName query
  seasonData <- TMDB.fetchTVSeason (TMDB.tvID showResults) seasonNum
  return (showResults, seasonData)

-- Search given query for a show name, return data for the first show returned
-- by the API.
firstMatchForName :: T.Text -> TMDB.TheMovieDB TMDB.TV
firstMatchForName query = do
  results <- TMDB.searchTV query
  return (head results) -- TODO: not safe, of course

-- Map any Error type to our internal type in Eithers.
mapError :: Show a => Either a b -> Either APIError b
mapError (Right x) = Right x
mapError (Left e) = Left $ T.pack $ show e

-- Remove traces of the TMDB types for both success and error responses.
-- TODO: Map error type to an internal type as well, probably. For now, just
-- use Text. All we're doing from here is printing it anyway.
mapEither :: Either TMDB.Error [TMDB.TV] -> Either APIError [TvShow]
mapEither (Right s) = Right (map mapTvShow s)
mapEither (Left e) = Left $ T.pack $ show e
