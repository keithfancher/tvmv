module Main (main) where

import API (APIKey, searchShowByName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Show (printShows)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (apiKey, query) = processArgs args
  result <- searchShowByName apiKey query
  case result of
    Left e -> TIO.putStrLn e
    Right tvShows -> printShows tvShows

-- Obviously not a long-term solution...
processArgs :: [String] -> (APIKey, T.Text)
processArgs [k, q] = (T.pack k, T.pack q)
processArgs _ = error "Usage: tvmv [APIKEY] [QUERY]"
