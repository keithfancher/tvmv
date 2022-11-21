module Command
  ( Command (..),
    MvOptions (..),
    SearchKey (..),
    SearchOptions (..),
    UndoOptions (..),
  )
where

import API (APIKey)
import qualified Data.Text as T
import Domain.Show (ItemId)
import File (InFiles)

-- The three commands
data Command
  = Mv MvOptions
  | Search SearchOptions
  | Undo UndoOptions

-- Search by name or by ID
data SearchKey = Name T.Text | Id ItemId

data MvOptions = MvOptions
  { -- APIKey might not be present in the CLI options. If this is Nothing, we'll
    -- check an env var and a file down the road. But it IS required eventually.
    apiKey :: Maybe APIKey,
    force :: Bool, -- do not wait for user confirmation
    noLog :: Bool, -- don't write a log file
    searchKey :: SearchKey,
    seasonNum :: Int,
    seasonFiles :: InFiles
  }

data SearchOptions = SearchOptions
  { apiKey :: Maybe APIKey,
    query :: T.Text
  }

data UndoOptions = UndoOptions
  { force :: Bool,
    -- if a logFile is't specified, we'll try to read the most recent one in the current directory
    logFile :: Maybe FilePath
  }
