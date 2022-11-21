module API.TMDBSpec (spec) where

import API.TMDB
import Domain.Show (Episode (..), Season (..), TvShow (..))
import qualified Network.API.TheMovieDB as TMDB
import Test.Hspec

spec :: Spec
spec = do
  describe "mapTvShow" $ do
    it "maps a TV show and all its episodes into our domain" $ do
      mapTvShow tmdbShow `shouldBe` resultShow

tmdbShow :: TMDB.TV
tmdbShow =
  TMDB.TV
    { TMDB.tvID = 12345,
      TMDB.tvName = "Buffy",
      TMDB.tvOverview = "Good stuff",
      TMDB.tvGenres = [],
      TMDB.tvPopularity = 100,
      TMDB.tvPosterPath = "poster/path/here",
      TMDB.tvFirstAirDate = Nothing,
      TMDB.tvLastAirDate = Nothing,
      TMDB.tvNumberOfSeasons = 7,
      TMDB.tvNumberOfEpisodes = 666,
      TMDB.tvSeasons = [tmdbSeason]
    }

tmdbSeason :: TMDB.Season
tmdbSeason =
  TMDB.Season
    { TMDB.seasonID = 123,
      TMDB.seasonNumber = 1,
      TMDB.seasonAirDate = Nothing,
      TMDB.seasonEpisodeCount = 22,
      TMDB.seasonPosterPath = "whatever",
      TMDB.seasonEpisodes = [tmdbEpisode]
    }

tmdbEpisode :: TMDB.Episode
tmdbEpisode =
  TMDB.Episode
    { TMDB.episodeID = 9000,
      TMDB.episodeNumber = 12,
      TMDB.episodeName = "Best ep",
      TMDB.episodeOverview = "Stuff happened",
      TMDB.episodeSeasonNumber = 1,
      TMDB.episodeAirDate = Nothing,
      TMDB.episodeStillPath = "path/to/still"
    }

resultShow :: TvShow
resultShow =
  TvShow
    { showId = 12345,
      showName = "Buffy",
      seasons = [resultSeason],
      description = "Good stuff",
      numberOfSeasons = 7,
      numberOfEpisodes = 666
    }

resultSeason :: Season
resultSeason = Season {seasonNumber = 1, episodes = [resultEpisode]}

resultEpisode :: Episode
resultEpisode =
  Episode
    { episodeNumber = 12,
      episodeName = "Best ep",
      episodeSeasonNumber = 1,
      episodeShowName = "Buffy"
    }
