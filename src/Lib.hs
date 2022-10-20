module Lib (someFunc) where

import qualified Data.Text as T
import Network.API.TheMovieDB

someFunc :: IO ()
someFunc = searchTvShow "APIKEY" "buffy"

searchTvShow :: T.Text -> T.Text -> IO ()
searchTvShow key query = do
  result <- runTheMovieDB (defaultSettings key) (searchTV query)
  print result
