module Args (cliOptParser) where

import API (APIKey)
import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import qualified Data.Text as T
import File (InFiles, mkInFiles)
import Options.Applicative

cliOptParser :: ParserInfo Command
cliOptParser =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> header "tvmv: Bulk-rename your TV episode files with minimal fuss."
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
    <*> searchKeyParser
    <*> option
      auto
      ( long "season"
          <> short 's'
          <> help "The season number for the files you're renaming. tvmv operates in units of seasons."
          <> metavar "SEASON_NUM"
      )
    <*> inFilesParser

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


maybeApiKeyReader :: ReadM (Maybe APIKey)
maybeApiKeyReader = eitherReader $ \s ->
  case s of
    "" -> Right Nothing
    anythingNonEmpty -> Right $ Just $ T.pack anythingNonEmpty

inFilesParser :: Parser InFiles
inFilesParser = mkInFiles <$> filePathsParser

-- We get these in from the CLI as a list of paths, but need to transform it
-- into `InFiles`, the actually-useful type.
filePathsParser :: Parser [FilePath]
filePathsParser =
  many -- `many` == "zero or more"
    ( argument
        str
        ( metavar "DIR_PATH|FILES"
            <> help "If omitted, tvmv will operate on all files in the current directory. Otherwise, you can specify EITHER a single directory OR a set of files (via globbing or whatever else)."
        )
    )

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
maybeLogFileReader = eitherReader $ \s ->
  case s of
    "" -> Right Nothing
    anythingNonEmpty -> Right $ Just anythingNonEmpty
