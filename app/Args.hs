module Args (cliOptParser) where

import Command (Command (..), MvOptions (..), SearchKey (..), SearchOptions (..), UndoOptions (..))
import Options.Applicative

mvOptionsParser :: Parser MvOptions
mvOptionsParser =
  MvOptions
    <$> strOption
      ( long "api-key"
          <> short 'k'
          <> metavar "API_KEY"
          <> help "..." -- TODO
      )
    <*> searchKeyParser
    <*> option
      auto
      ( long "season"
          <> short 's'
          <> help "..." -- TODO
          <> metavar "SEASON_NUM"
      )
    <*> many -- `many` == "zero or more"
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
    <$> strOption
      ( long "api-key"
          <> short 'k'
          <> metavar "API_KEY"
          <> help "..." -- TODO
      )
    <*> argument
      str
      ( metavar "SHOW_NAME"
          <> help "..." -- TODO
      )

undoOptionsParser :: Parser UndoOptions
undoOptionsParser =
  UndoOptions
    <$> argument -- TODO: optional, search for (latest?) log file in dir if not specified
      str
      ( metavar "TVMV_LOG_FILE"
          <> help "..." -- TODO
      )

mvCommandParser :: Parser Command
mvCommandParser = Mv <$> mvOptionsParser

searchCommandParser :: Parser Command
searchCommandParser = Search <$> searchOptionsParser

undoCommandParser :: Parser Command
undoCommandParser = Undo <$> undoOptionsParser

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

cliOptParser :: ParserInfo Command
cliOptParser =
  info
    (commandParser <**> helper)
    ( fullDesc -- TODO: update description!
        <> progDesc "A more detailed descrption here. Blah blah blah!"
        <> header "Head text here. Brief description of what this even is."
    )
