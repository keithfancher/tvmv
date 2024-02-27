module Parse
  ( SeasonEpNum (..),
    parseFilename,
  )
where

import System.FilePath (takeFileName)
import Text.Parsec (ParseError, anyChar, char, lookAhead, many, many1, manyTill, parse, try, (<|>))
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
parseFilename fullFilePath = parse fullFilename err fileName
  where
    fileName = takeFileName fullFilePath -- Strip the leading path, if it exists
    err = "" -- used only in errors, we don't need it

-- Parses out the season number and episode number, ignoring all leading and
-- trailing characters.
fullFilename :: Parser SeasonEpNum
fullFilename = withLeadingAndTrailingChars seasonEpNum
  where
    -- NOTE: The only reason none of these (currently) needs a `try` is that
    -- there is no overlap in the first character they parse, so they'll never
    -- consume each others' input. If this changes, don't forget the `try`!
    seasonEpNum = seasonEpNumXFormat <|> seasonEpNumSEFormat <|> seasonEpNumEpOnlyFormat

-- Use the given parser, but allow any number of leading and trailing characters.
withLeadingAndTrailingChars :: Parser a -> Parser a
withLeadingAndTrailingChars parser = do
  _ <- leadingChars
  value <- parser
  _ <- many anyChar
  return value
  where
    -- Consume characters until we succeed in parsing a value. We need a
    -- version of our parser that doesn't *consume* our actual value here,
    -- since `manyTill` throws that bit away.
    leadingChars = manyTill anyChar parserLookAhead
    -- The combination of `try` and `lookAhead` allows checking for a
    -- successful parse without consuming any input:
    parserLookAhead = try $ lookAhead parser

-- Consumes, e.g., "2x12"
seasonEpNumXFormat :: Parser SeasonEpNum
seasonEpNumXFormat = do
  s <- parseInt
  _ <- char 'x' <|> char 'X'
  e <- parseInt
  return SeasonEpNum {seasonNum = s, episodeNum = e}

-- Consumes, e.g., "s02e12"
seasonEpNumSEFormat :: Parser SeasonEpNum
seasonEpNumSEFormat = do
  _ <- char 's' <|> char 'S'
  s <- parseInt
  _ <- char 'e' <|> char 'E'
  e <- parseInt
  return SeasonEpNum {seasonNum = s, episodeNum = e}

-- Consumes, e.g., "EP24". Often used for shows with only a single season.
-- Implies season 1.
seasonEpNumEpOnlyFormat :: Parser SeasonEpNum
seasonEpNumEpOnlyFormat = do
  _ <- char 'e' <|> char 'E'
  _ <- char 'p' <|> char 'P'
  e <- parseInt
  return SeasonEpNum {seasonNum = 1, episodeNum = e}

parseInt :: Parser Int
parseInt = do
  n <- many1 digit
  return (read n)
