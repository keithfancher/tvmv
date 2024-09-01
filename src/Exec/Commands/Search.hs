module Exec.Commands.Search (search) where

import API qualified
import Command (SearchOptions (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Domain.API (APIWrapper)
import Exec.Env (Env)
import Print.Color (Colorized (..))
import Text.Printf (printf)

-- Query the configured API for a show with the given name. The `search` command!
search :: (MonadIO m) => Env -> APIWrapper m -> SearchOptions -> m ()
search env withApi (SearchOptions maybeApiKey searchQuery) = do
  key <- API.resolveAPIKey maybeApiKey env
  putStrLn' "Querying API..."
  tvShowResults <- API.searchShowByName withApi key searchQuery
  putStrLn' $ resultsMsg tvShowResults
  printColorizedListLn tvShowResults
  where
    resultsMsg r = printf "Found %d results\n" (length r)

-- Wrapper for less lifting :')
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn
