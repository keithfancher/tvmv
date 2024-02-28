module API.TMDB
  ( mapTvShow,
    tmdbApiWrapper,
  )
where

import API.Error (toAPIError)
import Data.Text qualified as T
import Domain.API (APIKey, APIWrapper (..))
import Domain.Error (Error (..))
import Domain.Show (Episode (..), ItemId, Season (..), TvShow (..))
import Monad.Tvmv (Tvmv, mkTvmv)
import Network.API.TheMovieDB qualified as TMDB

tmdbApiWrapper :: APIWrapper Tvmv
tmdbApiWrapper =
  APIWrapper
    { getShow = getShow',
      getSeason = getSeason',
      queryShows = queryShows'
    }

-- Get data for a single show. Note that this only fetches certain top-level
-- data for the show, it does NOT get all the season/episode data.
getShow' :: APIKey -> ItemId -> Tvmv TvShow
getShow' apiKey itemId = toTvmv apiKey opDescription $ getShowTMDB itemId
  where
    opDescription = "fetch show data for ID: " <> toText itemId

-- Given the show data, we can pull down the data for a given season, and all
-- its episodes.
getSeason' :: APIKey -> TvShow -> Int -> Tvmv Season
getSeason' apiKey showData seasonNum = toTvmv apiKey opDescription $ getSeasonTMDB showData seasonNum
  where
    opDescription = "fetch season " <> toText seasonNum <> " of " <> showInfo
    showInfo = "\"" <> showName showData <> "\" (ID: " <> toText (showId showData) <> ")"

-- Get list of shows from TMDB that match the given query. Again, this only
-- contains the top-level data for each returned show.
queryShows' :: APIKey -> T.Text -> Tvmv [TvShow]
queryShows' apiKey nameQuery = toTvmv apiKey opDescription $ queryShowsTMDB nameQuery
  where
    opDescription = "search shows with query text \"" <> nameQuery <> "\""

getShowTMDB :: ItemId -> TMDB.TheMovieDB TvShow
getShowTMDB itemId = do
  showData <- TMDB.fetchTV itemId
  return $ mapTvShow showData

getSeasonTMDB :: TvShow -> Int -> TMDB.TheMovieDB Season
getSeasonTMDB showData seasonNum = do
  seasonData <- TMDB.fetchTVSeason (showId showData) seasonNum
  let name = showName showData -- All we really need from the show here
  return $ mapTvSeason name seasonData

queryShowsTMDB :: T.Text -> TMDB.TheMovieDB [TvShow]
queryShowsTMDB nameQuery = do
  tvdbShows <- TMDB.searchTV nameQuery
  return $ map mapTvShow tvdbShows

-- Run it! Requires an API key.
runTMDB :: APIKey -> TMDB.TheMovieDB a -> IO (Either TMDB.Error a)
runTMDB key = TMDB.runTheMovieDB (TMDB.defaultSettings key)

-- Map to our internal monad stack, converting errors along the way.
-- The `opDescription` parameter is used only in the case of errors, so we can
-- tell the user exactly what we *attempted* to do when we failed.
toTvmv :: APIKey -> T.Text -> TMDB.TheMovieDB a -> Tvmv a
toTvmv key opDescription x = mkTvmv mappedResult
  where
    mappedResult = mapError opDescription <$> tmdbIO
    tmdbIO = runTMDB key x

-- Map Eithers to our internal Error type.
mapError :: T.Text -> Either TMDB.Error a -> Either Error a
mapError _ (Right r) = Right r
mapError opDescription (Left e) = Left $ appendOp $ toAPIError e
  where
    appendOp (APIError err) =
      APIError $ err <> "\nWhile attempting to " <> T.unpack opDescription
    appendOp otherError = otherError -- we don't care about non-API errors

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
      numberOfEpisodes = TMDB.tvNumberOfEpisodes s,
      showUrl = Just $ buildUrl (TMDB.tvID s)
    }
  where
    mapSeason = mapTvSeason (TMDB.tvName s) -- map with the given name
    buildUrl showId = "https://www.themoviedb.org/tv/" <> toText showId

toText :: (Show a) => a -> T.Text
toText = T.pack . show

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
