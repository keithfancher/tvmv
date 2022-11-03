module Show
  ( Episode (..),
    Season (..),
    TvShow (..),
    ItemId,
    printShows,
    showInfoBrief,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- A unique ID used for a given API resource.
--
-- TODO: Int or Text? Both TheTVDB and TheMovieDB use integers, though IMDB's
-- API would require a string. Text is probably most flexible, would require
-- fewer code changes if swapping out APIs. Probably change this and map
-- accordingly.
type ItemId = Int

data TvShow = TvShow
  { showId :: ItemId,
    showName :: T.Text,
    seasons :: [Season],
    description :: T.Text, -- Some "flavor text"
    numberOfSeasons :: Int,
    numberOfEpisodes :: Int
  }
  deriving (Eq, Show)

data Season = Season
  { seasonNumber :: Int,
    episodes :: [Episode]
  }
  deriving (Eq, Show)

-- Note that adding season number and show name to this structure is
-- technically redundant, but it also means an `Episode` is self-contained, has
-- all the data we need to operate.
data Episode = Episode
  { episodeNumber :: Int,
    episodeName :: T.Text,
    episodeSeasonNumber :: Int,
    episodeShowName :: T.Text
  }
  deriving (Eq, Show)

-- Brief text summary of a show.
showInfoBrief :: TvShow -> T.Text
showInfoBrief s =
  "ID: "
    <> toText (showId s)
    <> "\nName: "
    <> showName s
    <> "\nDescription: "
    <> description s

-- Print out summaries for a list of shows.
printShows :: [TvShow] -> IO ()
printShows s = TIO.putStrLn (T.intercalate sep showSummaries)
  where
    showSummaries = map showInfoBrief s
    sep = "\n--\n"

toText :: Show a => a -> T.Text
toText = T.pack . show
