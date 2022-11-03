module Error (Error (..)) where

data Error
  = APIError String
  | RenameError String
  | UndoError String
  deriving (Eq, Show)
