module Domain.Show
  ( Episode (..),
    Season (..),
    TvShow (..),
    ItemId,
    showInfoBrief,
  )
where

import qualified Data.Text as T

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
  "ID:\t"
    <> toText (showId s)
    <> "\nName:\t"
    <> showName s
    <> "\nBlurb:\t"
    <> trim (description s)
  where
    trim str
      | T.length str > 80 = T.take 78 str <> "..."
      | otherwise = str

toText :: Show a => a -> T.Text
toText = T.pack . show
