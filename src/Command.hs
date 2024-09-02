module Command
  ( Command (..),
    MvOptions (..),
    SearchKey (..),
    SearchOptions (..),
    UndoOptions (..),
    UserSearchTerms (..),
  )
where

import Data.Text (Text)
import Domain.API (APIKey)
import Domain.Show (ItemId)

-- The three commands
data Command
  = Mv MvOptions
  | Search SearchOptions
  | Undo UndoOptions

data MvOptions = MvOptions
  { -- APIKey might not be present in the CLI options. If this is Nothing, we'll
    -- check an env var and a file down the road. But it IS required eventually.
    apiKey :: Maybe APIKey,

    -- Boolean flags. These all default to False.
    force :: Bool, -- do not wait for user confirmation
    noLog :: Bool, -- don't write a log file
    allowPartial :: Bool, -- allow partial matches of episodes/files
    unicodeFilenames :: Bool, -- allow full unicode in resulting filenames

    -- If the user specifies NO search terms, we'll attempt to autodetect everything.
    -- If the user specifies ONLY show name/ID, we'll attempt to autodetect the season.
    -- If the user specifies a season, they MUST also specify a show (using ID or name).
    userSearchTerms :: Maybe UserSearchTerms,

    -- If the user specifies no files, we default to "all files in current dir".
    seasonFiles :: [FilePath]
  }

-- IF this is defined at all, it MUST contain a showSelection. However, you can
-- select a show but still leave off season number, allow it to be autodetected.
-- EXAMPLES:
--  `tvmv mv`                 // autodetect show name and season
--  `tvmv mv -n poirot`       // autodetect season
--  `tvmv mv -n poirot -s 4`  // don't autodetect anything
--  `tvmv mv -i 1234`         // using ID, autodetect season
--  `tvmv mv -i 1234 -s 4`    // using ID, don't autodetect anything
--  **BAD**, show not specified, can't autodetect: `tvmv mv -s 4`
--  (Note that the "bad" case is impossible to represent with this data model!)
data UserSearchTerms = UserSearchTerms
  { showSelection :: SearchKey,
    seasonSelection :: Maybe Int
  }

-- Search by name or by ID
data SearchKey = Name Text | Id ItemId

data SearchOptions = SearchOptions
  { apiKey :: Maybe APIKey,
    query :: Text
  }

data UndoOptions = UndoOptions
  { force :: Bool,
    -- if a logFile isn't specified, we'll try to read the most recent one in the current directory
    logFile :: Maybe FilePath
  }
