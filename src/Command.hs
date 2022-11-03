module Command
  ( Command (..),
    MvOptions (..),
    SearchKey (..),
  )
where

import API (APIKey)
import qualified Data.Text as T
import Show (ItemId)

-- The three commands
data Command
  = Mv MvOptions
  | Search SearchOptions
  | Undo UndoOptions

-- Search by name or by ID
data SearchKey = Name T.Text | Id ItemId

data MvOptions = MvOptions
  { mvApiKey :: APIKey,
    searchKey :: SearchKey,
    seasonNum :: Int,
    seasonDir :: FilePath
    -- TODO: flags, maybe a separate data struct, func to get defaults?
  }

data SearchOptions = SearchOptions
  { searchApiKey :: APIKey, -- TODO: name conflict, need that extension so we can remove stupid prefix
    query :: T.Text
  }

data UndoOptions = UndoOptions
  { logFile :: FilePath
  }
