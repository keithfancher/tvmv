module Filenames
  ( makePortable,
    makeVeryPortable,
  )
where

import Data.Char (isAlphaNum, isAscii, isPrint)
import Data.Map qualified as Map
import System.FilePath (makeValid)
import System.FilePath.Windows qualified as Win

-- Make filenames portable, AKA "make Windows-friendly":
--
-- 1. First pass to do some "nice" replacements, swapping out characters that
--    have easy ASCII equivalents ('é' for 'e', e.g.).
-- 2. Next, catch any remaining non-printable, non-ASCII characters.
-- 3. Another pass to remove Windows reserved characters and filenames.
-- 4. Another, final call to `makeValid` for whatever the current system is.
--    This ensures that, if by some wild chance, valid Windows is *not* a subset
--    of valid filenames for the current system, the final result will still be
--    valid. But in our current universe, this is almost certainly a no-op.
--
-- Note that Windows *does* allow Unicode in its filenames, but the Windows
-- command-line doesn't tend to handle it gracefully. Easier for everyone to
-- filter out everything but ASCII.
--
-- Also note: Windows is the most restrictive with its filenames (of our
-- supported operating systems). Valid Windows filenames should *also* be valid
-- Linux/MacOS filenames. (For base filenames -- not accounting for path
-- separators and so on.)
--
-- See:
--   - https://stackoverflow.com/a/35352640
--   - https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions
--   - https://en.wikipedia.org/wiki/Comparison_of_file_systems
makePortable :: FilePath -> FilePath
makePortable = makeValid . Win.makeValid . map makePrintableAscii . replaceFancyChars -- Note, called in reverse order
  where
    makePrintableAscii c =
      if isAscii c && isPrint c -- No control characters, no Unicode, etc.
        then c
        else '-'

-- Make filenames VERY portable, using these characters only:
--   [A–Z] [a–z] [0–9] ._-
-- As defined here:
--   https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_282
-- Probably more restrictive than anyone wants or needs. But here it is!
--
-- Note that we also call the various `makeValid` functions here to catch
-- Windows reserved filenames, etc.
makeVeryPortable :: FilePath -> FilePath
makeVeryPortable = makeValid . Win.makeValid . map makeCharVeryPortable . replaceFancyChars
  where
    makeCharVeryPortable c = if isVeryPortable c then c else '-'
    isVeryPortable c = isAscii c && (isAlphaNum c || isPortableSymbol c)
    isPortableSymbol c = c `elem` ['.', '-', '_']

-- Replace "fancy" characters in a path with less-fancy ASCII equivalents.
replaceFancyChars :: FilePath -> FilePath
replaceFancyChars = map replaceChar
  where
    -- If the char is in our map, replace. Otherwise, leave as-is.
    replaceChar c = Map.findWithDefault c c replacementMap

-- A map of character replacements, from "fancy" to "less-fancy". Winds up
-- looking something like:
--   á -> a
--   ä -> a
--   é -> e
--   ç -> c
--   etc...
--
-- Note that this is FAR from a complete mapping. Just some common low-hanging
-- fruit.
--
-- TODO, doubles: Æ æ Œ œ
-- TODO: match case of input (currently maps all to lower-case)
replacementMap :: Map.Map Char Char
replacementMap = Map.fromList allReplacementTuples
  where
    allReplacementTuples =
      genReplacementTuples aLike 'a'
        ++ genReplacementTuples eLike 'e'
        ++ genReplacementTuples iLike 'i'
        ++ genReplacementTuples oLike 'o'
        ++ genReplacementTuples uLike 'u'
        ++ genReplacementTuples yLike 'y'
        ++ otherReplacements
    aLike = "ÀÁÂÃÄÅàáâãäå"
    eLike = "ÈÉÊËèéêë"
    iLike = "ÌÍÎÏìíîï"
    oLike = "ÒÓÔÕÖØòóôõöø"
    uLike = "ÙÚÛÜùúûü"
    yLike = "ÝŸýÿ"
    otherReplacements =
      [ ('Ñ', 'N'),
        ('ñ', 'n'),
        ('Ç', 'C'),
        ('ç', 'c'),
        (':', '-'),
        -- replace double quotes with single quote:
        ('\"', '\''),
        -- "smart" quotes, replace with single quote:
        ('“', '\''),
        ('”', '\''),
        ('‘', '\''),
        ('’', '\''),
        -- fancy dashes, replace with hyphen:
        ('—', '-'), -- em dash
        ('–', '-') -- en dash
      ]

genReplacementTuples :: [a] -> a -> [(a, a)]
genReplacementTuples mapFromList mapToItem = map (,mapToItem) mapFromList
