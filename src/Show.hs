module Show
  ( TvShow (..),
    Season,
    Episode,
  )
where

import qualified Data.Text as T

type ShowId = Int

data TvShow = TvShow
  { showId :: ShowId,
    showName :: T.Text,
    seasons :: [Season],
    description :: T.Text -- Some "flavor text"
  }
  deriving (Eq, Show)

data Season = Season
  { seasonNum :: Int,
    episodes :: [Episode]
  }
  deriving (Eq, Show)

data Episode = Episode
  { epNumber :: Int,
    epName :: T.Text
  }
  deriving (Eq, Show)
