module Domain.Error
  ( Error (..),
    errorMessage,
  )
where

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
