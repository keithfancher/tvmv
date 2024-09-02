module Parse
  ( EpisodeData (..),
    SeasonEpNum (..),
    parseFilename,
  )
where

import Data.Char (isAlphaNum)
import Data.Either (isRight)
import Data.Text (Text, dropWhileEnd, pack, replace, strip)
import Domain.Error (Error (..))
import System.FilePath (takeFileName)
import Text.Parsec (anyChar, char, lookAhead, many, many1, manyTill, parse, try, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

-- The episode data we can parse out of a single filename. Show name may or may
-- not exist, but if we're parsing at all, `SeasonEpNum` must exist. (Also
-- since show name is parsed relative to that -- as the leading characters.)
data EpisodeData = EpisodeData
  { seasonEpNum :: SeasonEpNum,
    showName :: Maybe Text
  }
  deriving (Eq, Show)

-- Simple wrapper for the season number and episode number of an episode
data SeasonEpNum = SeasonEpNum
  { seasonNum :: Int,
    episodeNum :: Int
  }
  deriving (Eq, Show)

data MultiEpNums = MultiEpNums
  { seasonNum :: Int,
    episodeNums :: [Int]
  }
  deriving (Eq, Show)

-- Given the filename (or full path) for an episode, attempt to parse out the
-- season and episode number.
parseFilename :: FilePath -> Either Error EpisodeData
parseFilename fullFilePath =
  if isMultiEpFile fileName
    then Left $ ParseError "Multi-episode files are not yet supported :'("
    else mapParseResult $ parse fullFilename err fileName
  where
    fileName = takeFileName fullFilePath -- Strip the leading path, if it exists
    err = "" -- used only in errors, we don't need it
    mapParseResult (Left e) = Left $ ParseError $ show e
    mapParseResult (Right r) = Right r

isMultiEpFile :: String -> Bool
isMultiEpFile fileName = isRight parseMultiEp
  where
    parseMultiEp = parse (withLeadingAndTrailingChars multiEpFormat) "" fileName

-- Parses out the season number and episode number, ignoring all leading and
-- trailing characters.
fullFilename :: Parser EpisodeData
fullFilename = do
  (leadingChars, seasonEp) <- withLeadingAndTrailingChars seasonEpNum
  -- Show name is just a cleaned up version of the leading characters:
  let cleanName = case cleanShowName leadingChars of
        "" -> Nothing
        nonEmpty -> Just nonEmpty
  return $ EpisodeData {seasonEpNum = seasonEp, showName = cleanName}
  where
    -- NOTE: The only reason none of these (currently) needs a `try` is that
    -- there is no overlap in the first character they parse, so they'll never
    -- consume each others' input. If this changes, don't forget the `try`!
    seasonEpNum = seasonEpNumXFormat <|> seasonEpNumSEFormat <|> seasonEpNumEpOnlyFormat

-- Some simple transformations to make the show name more searchable/sane. Note
-- the preference for `Text` over `String`, since it handles Unicode better.
cleanShowName :: String -> Text
cleanShowName = strip . replacePeriods . stripTrailingJunk . pack
  where
    -- Any lingering "stuff" after the show name-proper:
    stripTrailingJunk = dropWhileEnd (not . isAlphaNum)
    -- Convention of using '.' as word separator. Why is that, anyway?
    replacePeriods = replace "." " "

-- Use the given parser, but allow any number of leading and trailing characters.
-- Return the leading characters along with the parsed result.
withLeadingAndTrailingChars :: Parser a -> Parser (String, a)
withLeadingAndTrailingChars parser = do
  leading <- leadingChars
  value <- parser
  _trailing <- many anyChar
  return (leading, value)
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

multiEpFormat :: Parser MultiEpNums
multiEpFormat = do
  _ <- char 's' <|> char 'S'
  s <- parseInt
  eps <- epRange
  return MultiEpNums {seasonNum = s, episodeNums = eps}

epRange :: Parser [Int]
epRange = try epRangeValid <|> try epRangeInvalid

-- e.g. "e23-e25".
epRangeValid :: Parser [Int]
epRangeValid = do
  _ <- char 'e' <|> char 'E'
  start <- parseInt
  _ <- char '-'
  _ <- char 'e' <|> char 'E'
  end <- parseInt
  return [start .. end]

-- e.g. "e23-25". (Note: missing the second 'e'.) Technically this is NOT
-- valid, but it's common enough in the wild that we'd better handle it.
epRangeInvalid :: Parser [Int]
epRangeInvalid = do
  _ <- char 'e' <|> char 'E'
  start <- parseInt
  _ <- char '-'
  end <- parseInt
  return [start .. end]

parseInt :: Parser Int
parseInt = do
  n <- many1 digit
  return (read n)
