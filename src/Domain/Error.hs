module Domain.Error
  ( Error (..),
    errorMessage,
    printErrorMessage,
  )
where

import Print.Color (asError, asWarning)

data Error
  = APIError String
  | RenameError String
  | UndoError String
  | MissingAPIKey
  | ParseError String
  | UserAbort
  deriving (Eq, Show)

-- User-friendly error messages.
errorMessage :: Error -> String
errorMessage UserAbort = "Operation aborted! No files have been renamed."
errorMessage (APIError msg) = "TMDB API error: " <> msg
errorMessage MissingAPIKey = "Unable to find TMDB API key!\nPlease provide a key via the '-k' CLI arg, the 'TMDB_API_KEY' env var, or a config file. See the README for more details. "
errorMessage otherError = "Failed with: " <> show otherError

printErrorMessage :: Error -> IO ()
printErrorMessage e = case e of
  UserAbort -> asWarning printMsg -- User abort is not an "error", as such... a warning!
  _otherErr -> asError printMsg
  where
    printMsg = putStrLn $ errorMessage e
