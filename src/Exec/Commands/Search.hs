module Exec.Commands.Search (search) where

import API (searchShowByName)
import Command (SearchOptions (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Exec.Env (Env, populateAPIKey)
import Print.Color (Colorized (..))
import Text.Printf (printf)

-- Query the configured API for a show with the given name. The `search` command!
search ::
  (MonadIO m, MonadError Error m) =>
  Env ->
  APIWrapper m ->
  SearchOptions ->
  m ()
search env withApi (SearchOptions maybeApiKey searchQuery) = do
  key <- liftEither $ populateAPIKey maybeApiKey env
  putStrLn' "Querying API..."
  tvShowResults <- searchShowByName withApi key searchQuery
  putStrLn' $ resultsMsg tvShowResults
  printColorizedListLn tvShowResults
  where
    resultsMsg r = printf "Found %d results\n" (length r)

-- Wrapper for less lifting :')
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = liftIO . putStrLn
