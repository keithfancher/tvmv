module Domain.Show
  ( Episode (..),
    Season (..),
    TvShow (..),
    ItemId,
    showInfoBrief,
  )
where

import Data.Text qualified as T
import Print.Color (Colorized (..), green, mono, uncolor, yellow)
import Text.Wrap (defaultWrapSettings, wrapTextToLines)

-- A unique ID used for a given API resource.
--
-- If/when I add other APIs in the future, might want to change this to a
-- string and map accordingly. Both TheTVDB and TheMovieDB use integer IDs,
-- though IMDB's API would require a string. String is most flexible, would
-- require fewer code changes if swapping out APIs.
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

instance Colorized TvShow where
  colorize s =
    "ID:\t"
      <> green (toText (showId s))
      <> "\nName:\t"
      <> yellow (showName s)
      <> "\nBlurb:\t"
      <> mono (wrapAndTrim (description s))
      <> url (showUrl s)
    where
      url Nothing = ""
      url (Just u) = mono $ "\nLink:\t" <> u

-- Brief text summary of a show.
--
-- For now, to avoid duplication, we're defining the colored text as the "base"
-- above, and UNcoloring it for the plain text version.
showInfoBrief :: TvShow -> T.Text
showInfoBrief = uncolor . colorize

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

toText :: (Show a) => a -> T.Text
toText = T.pack . show
