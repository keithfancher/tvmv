module Filenames
  ( makePortable,
    makeVeryPortable,
  )
where

import Data.Char (isAlphaNum, isAscii, isPrint)
import System.FilePath (makeValid)
import System.FilePath.Windows qualified as Win

-- Make filenames portable, AKA "make Windows-friendly":
--
-- 1. Ensure that only printable ASCII characters are used.
-- 2. Another pass to remove Windows reserved characters and filenames.
-- 3. Another, final call to `makeValid` for whatever the current system is.
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
makePortable = makeValid . Win.makeValid . map makePrintableAscii -- Note these are called in reverse order
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
makeVeryPortable = makeValid . Win.makeValid . map makeCharVeryPortable
  where
    makeCharVeryPortable c = if isVeryPortable c then c else '-'
    isVeryPortable c = isAscii c && (isAlphaNum c || isPortableSymbol c)
    isPortableSymbol c = c `elem` ['.', '-', '_']
