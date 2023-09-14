module Domain.Show
  ( Episode (..),
    Season (..),
    TvShow (..),
    ItemId,
    showInfoBrief,
  )
where

import Data.Text qualified as T
import Text.Wrap (defaultWrapSettings, wrapTextToLines)

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
    numberOfEpisodes :: Int,
    -- Can/should be populated by the API code, will be API-specific. If it's
    -- not populated (`Nothing`), the field will not be shown:
    showUrl :: Maybe T.Text
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
    <> wrapAndTrim (description s)
    <> url (showUrl s)
  where
    url Nothing = ""
    url (Just u) = "\nLink:\t" <> u

-- Format the show blurb for text console output. Wrap the text at 78 chars and
-- only grab the first 3 lines for longer results.
wrapAndTrim :: T.Text -> T.Text
wrapAndTrim t = trimmedLines <> suffix
  where
    trimmedLines = T.intercalate "\n\t" $ take numLinesToTake wrappedLines
    wrappedLines = wrapTextToLines defaultWrapSettings lineCharWidth t
    lineCharWidth = 78
    numLinesToTake = 3
    -- Show that we've cut something off, if that's the case:
    suffix = if length wrappedLines > numLinesToTake then "..." else ""

toText :: Show a => a -> T.Text
toText = T.pack . show
