module Main (main) where

import API (searchShowByName)
import Show (printShows)

main :: IO ()
main = do
  result <- searchShowByName apiKey "Buffy"
  case result of
    Left e -> print e
    Right tvShows -> printShows tvShows
  where
    apiKey = ""
