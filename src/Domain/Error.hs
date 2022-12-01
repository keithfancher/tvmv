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
  | UserAbort
  deriving (Eq, Show)

errorMessage :: Error -> String
-- Specal case for user abort, so it's a bit "nicer":
errorMessage UserAbort = "Operation aborted! No files have been renamed."
errorMessage otherError = "Failed with: " <> show otherError
