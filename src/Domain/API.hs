module Domain.API
  ( APIConfig (..),
    APIKey,
    APIWrapper (..),
    LanguageCode,
  )
where

import Data.Text (Text)
import Domain.Show (ItemId, Season, TvShow)

type APIKey = Text

-- e.g. "it" or "it-IT". Determines the language of the show and episode names
-- the API returns.
type LanguageCode = Text

-- Everything we send along with each API request.
data APIConfig = APIConfig
  { configKey :: APIKey,
    -- If this is Nothing, we get the API's default language (English).
    configLanguage :: Maybe LanguageCode
  }
  deriving (Eq, Show)

data APIWrapper m = APIWrapper
  { getShow :: APIConfig -> ItemId -> m TvShow,
    getSeason :: APIConfig -> TvShow -> Int -> m Season,
    queryShows :: APIConfig -> Text -> m [TvShow]
  }
