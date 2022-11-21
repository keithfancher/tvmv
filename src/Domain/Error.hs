module Domain.Error (Error (..)) where

data Error
  = APIError String
  | RenameError String
  | UndoError String
  | InvalidInput String
  | UserAbort
  deriving (Eq, Show)
