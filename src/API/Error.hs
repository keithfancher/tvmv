module API.Error (toAPIError) where

import Domain.Error qualified as Internal
import Network.API.TheMovieDB qualified as TMDB
import Text.Read (readMaybe)

-- SLIGHTLY nicer error-mapping. We're still getting back ugly `show`-ified
-- error strings in some cases, but are able to pull out user-friendly messages
-- in certain common cases.
toAPIError :: TMDB.Error -> Internal.Error
toAPIError TMDB.InvalidKeyError = Internal.APIError "Invalid TMDB API key!"
toAPIError (TMDB.ServiceError err) = Internal.APIError $ niceServiceError err
toAPIError err = Internal.APIError $ show err

-- A bit of a hack in the name of nicer error messages.
--
-- The `TMDB` library is just giving us back `show`-ified objects in the case
-- of errors. This `Status` type just mirrors `Network.HTTP.Types.Status` so we
-- can make an easy `Read` instance to pull the data back out and display in a
-- more user-friendly way.
data Status = Status
  { statusCode :: Int,
    statusMessage :: String
  }
  deriving (Eq, Show, Read)

-- A `TMDB.ServiceError` simply wraps a `Status` object. Attempt to parse out
-- the pieces.
--
-- This will totally break with changes to the TMDB library internals, so we
-- fall back to the `show`-ified string.
niceServiceError :: String -> String
niceServiceError errString = case readMaybe errString of
  Just (Status c m) -> "Error code " <> show c <> ", " <> m
  Nothing -> errString
