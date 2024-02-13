module Domain.Error
  ( Error (..),
    errorMessage,
  )
where

data Error
  = APIError String
  | RenameError String
  | UndoError String
  | InvalidInput String
  | ParseError String
  | UserAbort
  deriving (Eq, Show)

errorMessage :: Error -> String
-- Specal case for user abort, so it's a bit "nicer":
errorMessage UserAbort = "Operation aborted! No files have been renamed."
errorMessage (APIError msg) = "TMDB API error: " <> msg
errorMessage otherError = "Failed with: " <> show otherError
