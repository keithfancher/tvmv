module Domain.API
  ( APIKey,
    APIWrapper (..),
  )
where

import Data.Text qualified as T
import Domain.Show (ItemId, Season, TvShow)

type APIKey = T.Text

data APIWrapper m = APIWrapper
  { getShow :: APIKey -> ItemId -> m TvShow,
    getSeason :: APIKey -> TvShow -> Int -> m Season,
    queryShows :: APIKey -> T.Text -> m [TvShow]
  }
