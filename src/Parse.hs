module Parse
  ( SeasonEpNum (..),
    parseFilename,
  )
where

import Text.Parsec (ParseError, char, many1, parse, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

-- Simple wrapper for the season number and episode number of an episode
data SeasonEpNum = SeasonEpNum
  { seasonNum :: Int,
    episodeNum :: Int
  }
  deriving (Eq, Show)

-- Given the filename (or full path) for an episode, attempt to parse out the
-- season and episode number.
--
-- TODO: Actually, map this `ParseError` to an error in our domain. The calling
-- code shouldn't have to depend on Parsec!
parseFilename :: FilePath -> Either ParseError SeasonEpNum
parseFilename = parse parser err
  where
    parser = seasonEpNumXFormat <|> seasonEpNumSEFormat
    err = "" -- used only in errors, we don't need it

-- e.g. "2x12"
seasonEpNumXFormat :: Parser SeasonEpNum
seasonEpNumXFormat = do
  s <- parseInt
  _ <- char 'x'
  e <- parseInt
  return SeasonEpNum {seasonNum = s, episodeNum = e}

-- e.g. "s02e12"
seasonEpNumSEFormat :: Parser SeasonEpNum
seasonEpNumSEFormat = do
  _ <- char 's'
  s <- parseInt
  _ <- char 'e'
  e <- parseInt
  return SeasonEpNum {seasonNum = s, episodeNum = e}

parseInt :: Parser Int
parseInt = do
  n <- many1 digit
  return (read n)
