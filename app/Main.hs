module Main (main) where

import Args (cliOptParser)
import Command (MvOptions (..), SearchKey (..))
import Execute (renameSeason, run)
import Options.Applicative (execParser)

main :: IO ()
main = do
  (MvOptions apiKey query season dirPath) <- execParser cliOptParser
  result <- run $ renameSeason apiKey (getQueryName query) season dirPath
  print result
  where
    getQueryName (Name name) = name
    getQueryName (Id _) = error "TODO: not using IDs yet" -- TODO, natch
