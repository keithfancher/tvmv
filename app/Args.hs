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
    ( fullDesc -- TODO: update description!
        <> progDesc "A more detailed description here. Blah blah blah!"
        <> header "Header text here. Brief description of what this even is."
    )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "mv"
        ( info
            (mvCommandParser <**> helper)
            (progDesc "TODO")
        )
        <> command
          "search"
          ( info
              (searchCommandParser <**> helper)
              (progDesc "TODO")
          )
        <> command
          "undo"
          ( info
              (undoCommandParser <**> helper)
              (progDesc "TODO")
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
    <*> searchKeyParser
    <*> option
      auto
      ( long "season"
          <> short 's'
          <> help "..." -- TODO
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
        <> help "..." -- TODO
        <> value Nothing -- If not specified, it's Nothing
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
            <> help "..." -- TODO
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
          <> help "..." -- TODO
      )

idParser :: Parser SearchKey
idParser =
  Id
    <$> option
      auto
      ( long "id"
          <> short 'i'
          <> metavar "SHOW_ID"
          <> help "..." -- TODO
      )

searchOptionsParser :: Parser SearchOptions
searchOptionsParser =
  SearchOptions
    <$> apiKeyParser
    <*> argument
      str
      ( metavar "SHOW_NAME"
          <> help "..." -- TODO
      )

undoOptionsParser :: Parser UndoOptions
undoOptionsParser =
  UndoOptions
    <$> argument
      maybeLogFileReader
      ( metavar "TVMV_LOG_FILE"
          <> help "..." -- TODO
          <> value Nothing
      )

maybeLogFileReader :: ReadM (Maybe FilePath)
maybeLogFileReader = eitherReader $ \s ->
  case s of
    "" -> Right Nothing
    anythingNonEmpty -> Right $ Just anythingNonEmpty
