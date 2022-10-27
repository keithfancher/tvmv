module Error (Error (..)) where

data Error = APIError String | RenameError String
  deriving (Eq, Show)
