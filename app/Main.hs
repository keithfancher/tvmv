module Main (main) where

import API (searchShowByName)

main :: IO ()
main = do
  result <- searchShowByName apiKey "Buffy"
  print result
  where
    apiKey = ""
