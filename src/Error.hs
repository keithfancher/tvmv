module Error (Error (..)) where

data Error
  = APIError String
  | RenameError String
  | UndoError String
  | InvalidInput String
  deriving (Eq, Show)
