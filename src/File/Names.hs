module File.Names
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
-- 1. Remove annoying characters entirely. (Question marks!)
-- 2. A pass to do some "nice" replacements, swapping out characters that have
--    easy ASCII equivalents ('é' for 'e', e.g.).
-- 3. Catch any remaining non-printable, non-ASCII characters.
-- 4. Remove Windows reserved characters and filenames.
-- 5. Another, final call to `makeValid` for whatever the current system is.
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
-- Note, function composition means these are actually called from right to left:
makePortable = makeValid . Win.makeValid . map makePrintableAscii . replaceFancyChars . removals
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
makeVeryPortable = makeValid . Win.makeValid . map makeCharVeryPortable . replaceFancyChars . removals
  where
    makeCharVeryPortable c = if isVeryPortable c then c else '-'
    isVeryPortable c = isAscii c && (isAlphaNum c || isPortableSymbol c)
    isPortableSymbol c = c `elem` ['.', '-', '_']

-- Remove annoying characters. For now, this is just question marks. This may
-- be personal preference, but I'd rather have this:
--   `Is this an episode name?.mp4`
-- become this:
--   `Is this an episode name.mp4`
-- instead of something like this:
--   `Is this an episode name_.mp4`
removals :: FilePath -> FilePath
removals = filter (/= '?')

-- Replace "fancy" characters in a path with less-fancy ASCII equivalents.
replaceFancyChars :: FilePath -> FilePath
replaceFancyChars = map replaceChar . replaceDoubles
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
replacementMap :: Map.Map Char Char
replacementMap = Map.fromList allReplacementTuples
  where
    allReplacementTuples =
      genReplacementTuples "ÀÁÂÃÄÅ" 'A'
        ++ genReplacementTuples "àáâãäå" 'a'
        ++ genReplacementTuples "ÈÉÊË" 'E'
        ++ genReplacementTuples "èéêë" 'e'
        ++ genReplacementTuples "ÌÍÎÏ" 'I'
        ++ genReplacementTuples "ìíîï" 'i'
        ++ genReplacementTuples "ÒÓÔÕÖØ" 'O'
        ++ genReplacementTuples "òóôõöø" 'o'
        ++ genReplacementTuples "ÙÚÛÜ" 'U'
        ++ genReplacementTuples "ùúûü" 'u'
        ++ genReplacementTuples "ÝŸ" 'Y'
        ++ genReplacementTuples "ýÿ" 'y'
        ++ otherReplacements
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

-- Common "double" characters, e.g. "Æ æ Œ œ". Require an extra step because
-- they map from one char to two.
replaceDoubles :: FilePath -> FilePath
replaceDoubles = concatMap replace
  where
    replace 'Æ' = "AE"
    replace 'æ' = "ae"
    replace 'Œ' = "OE"
    replace 'œ' = "oe"
    replace otherChar = [otherChar]
