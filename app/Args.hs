module Args (cliOptParser) where

import Command (MvOptions (..), SearchKey (..))
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
    <*> strOption
      ( long "dir"
          <> short 'd'
          <> metavar "DIR_PATH"
          <> help "..." -- TODO
          <> showDefault
          <> value "." -- Default to current dir (TODO: make this a positional arg, probably)
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

-- TODO: not just mv options, of course
cliOptParser :: ParserInfo MvOptions
cliOptParser =
  info
    (mvOptionsParser <**> helper)
    ( fullDesc -- TODO: update description!
        <> progDesc "A more detailed descrption here. Blah blah blah!"
        <> header "Head text here. Brief description of what this even is."
    )
