module Args (cliOptParser) where

import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..), UserSearchTerms (..))
import Data.Text qualified as T
import Domain.API (APIKey)
import Options.Applicative
import Version (currentVersion)

cliOptParser :: ParserInfo Command
cliOptParser =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> header ("tvmv: Bulk-rename your TV episode files with minimal fuss. (v" <> currentVersion <> ")")
        <> progDesc "For help with a specific command, run: tvmv [COMMAND] -h"
    )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "mv"
        ( info
            (mvCommandParser <**> helper)
            (progDesc "Rename the files for a TV season using API data")
        )
        <> command
          "search"
          ( info
              (searchCommandParser <**> helper)
              (progDesc "Query the configured API for TV show data")
          )
        <> command
          "undo"
          ( info
              (undoCommandParser <**> helper)
              (progDesc "Undo a previously-run rename operation")
          )
    )

mvCommandParser :: Parser Command
mvCommandParser = Mv <$> mvOptionsParser

searchCommandParser :: Parser Command
searchCommandParser = Search <$> searchOptionsParser

undoCommandParser :: Parser Command
undoCommandParser = Undo <$> undoOptionsParser

mvOptionsParser :: Parser MvOptions
mvOptionsParser =
  MvOptions
    <$> apiKeyParser
    <*> forceFlagParser
    <*> noLogFlagParser
    <*> allowPartialParser
    <*> unicodeFilenamesParser
    <*> userSearchTermsParser
    <*> filePathsParser

-- Fully optional. If no search terms specified, tvmv will try to parse out
-- show name, season number, etc. Previously known as "auto" mode.
userSearchTermsParser :: Parser (Maybe UserSearchTerms)
userSearchTermsParser =
  optional $
    UserSearchTerms
      <$> searchKeyParser
      <*> maybeSeasonNumberParser

searchKeyParser :: Parser SearchKey
searchKeyParser = nameParser <|> idParser

nameParser :: Parser SearchKey
nameParser =
  Name
    <$> strOption
      ( long "name"
          <> short 'n'
          <> metavar "SHOW_NAME"
          <> help "The show name, or a fragment of the show name. We'll search the API with this query and use the first matching result to get show data."
      )

idParser :: Parser SearchKey
idParser =
  Id
    <$> option
      auto
      ( long "id"
          <> short 'i'
          <> metavar "SHOW_ID"
          <> help "If you have the show's unique ID, you can search with that directly, rather than using its name. (You can get a show's ID with `tvmv search`.)"
      )

maybeSeasonNumberParser :: Parser (Maybe Int)
maybeSeasonNumberParser =
  optional $
    option
      auto
      ( long "season"
          <> short 's'
          <> help "The season number for the files you're renaming. tvmv operates in units of seasons."
          <> metavar "SEASON_NUM"
      )

apiKeyParser :: Parser (Maybe APIKey)
apiKeyParser =
  option
    maybeApiKeyReader
    ( long "api-key"
        <> short 'k'
        <> metavar "API_KEY"
        <> help "Your TMDB API key. You can also pass this in via an env var or a file -- see the README for details!"
        <> value Nothing -- If not specified, it's Nothing
    )

forceFlagParser :: Parser Bool
forceFlagParser =
  switch
    ( long "force"
        <> short 'f'
        <> help "Do not wait for user confirmation before renaming files. (By default, tvmv will ask you before it makes any file changes.)"
    )

noLogFlagParser :: Parser Bool
noLogFlagParser =
  switch
    ( long "no-log"
        <> short 'x'
        <> help "Do not write a log of rename operations. (Note that without a log, you can't use the `undo` command.)"
    )

allowPartialParser :: Parser Bool
allowPartialParser =
  switch
    ( long "allow-partial"
        <> short 'p'
        <> help "Allow mismatched number of episodes/files. (i.e. you have an incomplete set of files.) See README for more details."
    )

unicodeFilenamesParser :: Parser Bool
unicodeFilenamesParser =
  switch
    ( long "unicode-filenames"
        <> short 'u'
        <> help "Allow full unicode set of characters in resulting filenames. (By default, tvmv will create portable, Windows-friendly, ASCII-only filenames.)"
    )

maybeApiKeyReader :: ReadM (Maybe APIKey)
maybeApiKeyReader = eitherReader $ \case
  "" -> Right Nothing
  anythingNonEmpty -> Right $ Just $ T.pack anythingNonEmpty

-- One directory OR zero or more file paths. If zero, operate on all files in current dir.
filePathsParser :: Parser [FilePath]
filePathsParser =
  many -- `many` == "zero or more"
    ( argument
        str
        ( metavar "DIR_PATH|FILES"
            <> help "If omitted, tvmv will operate on all files in the current directory. Otherwise, you can specify EITHER a single directory OR a set of files (via globbing or whatever else)."
        )
    )

searchOptionsParser :: Parser SearchOptions
searchOptionsParser =
  SearchOptions
    <$> apiKeyParser
    <*> argument
      str
      ( metavar "SHOW_NAME"
          <> help "The show name, or a fragment of the show name."
      )

undoOptionsParser :: Parser UndoOptions
undoOptionsParser =
  UndoOptions
    <$> forceFlagParser
    <*> argument
      maybeLogFileReader
      ( metavar "TVMV_LOG_FILE"
          <> help "A log file from a previous rename operation. If omitted, tvmv will look for the MOST RECENT log file in the CURRENT directory."
          <> value Nothing
      )

maybeLogFileReader :: ReadM (Maybe FilePath)
maybeLogFileReader = eitherReader $ \case
  "" -> Right Nothing
  anythingNonEmpty -> Right $ Just anythingNonEmpty
