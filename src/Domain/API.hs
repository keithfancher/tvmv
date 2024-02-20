module Domain.API
  ( APIKey,
    APIWrapper (..),
  )
where

import Data.Text (Text)
import Domain.Show (ItemId, Season, TvShow)

type APIKey = Text

data APIWrapper m = APIWrapper
  { getShow :: APIKey -> ItemId -> m TvShow,
    getSeason :: APIKey -> TvShow -> Int -> m Season,
    queryShows :: APIKey -> Text -> m [TvShow]
  }
