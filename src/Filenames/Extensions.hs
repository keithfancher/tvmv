module Filenames.Extensions (replaceBaseName') where

import Data.Set qualified as Set
import Language.Codes (iso639_1, iso639_2)
import System.FilePath (normalise, splitExtension, splitFileName, (<.>), (</>))

-- A custom version of `replaceBaseName` that accounts for subtitle language
-- metadata as part of the file extension. For example:
--
-- replaceBaseName' "foo.srt" "bar" -> "bar.srt"
-- replaceBaseName' "foo.en.srt" "bar" -> "bar.en.srt"
-- replaceBaseName' "/stuff/foo.en.sdh.forced.srt" "bar" -> "/stuff/bar.en.sdh.forced.srt"
--
-- This is following the Plex naming convention, more or less:
-- https://support.plex.tv/articles/200471133-adding-local-subtitles-to-your-media/#toc-3
replaceBaseName' :: FilePath -> String -> FilePath
replaceBaseName' fp newBase = normalise $ dir </> newBase <.> fullExt
  where
    (dir, fileName) = splitFileName fp
    fullExt = takeFullExtension fileName

-- Takes the FULL extension from a file, which in our case includes any
-- subtitle language metadata. For example:
--
-- takeFullExtension "blah.en.sdh.forced.srt" -> "en.sdh.forced.srt"
takeFullExtension :: FilePath -> FilePath
takeFullExtension fp = subMetadata <.> mainExt
  where
    (base, mainExt) = splitExtension fp
    subMetadata = takeAllSubMetadata base

-- Recursively works its way back through all the extensions in a filename as
-- long as they're valid sub language metadata. As soon as it hits an extension
-- that is NOT valid sub metadata, it returns what it's gotten so far.
takeAllSubMetadata :: FilePath -> FilePath
takeAllSubMetadata fp =
  if validSubMetadata ext
    then takeAllSubMetadata base <.> ext
    else ""
  where
    (base, ext) = splitExtension fp

-- Is the given extension a valid ISO-639-1 or ISO-639-2 language code?
validSubMetadata :: FilePath -> Bool
validSubMetadata ext =
  strippedExt `elem` predefinedValid
    || valid2Char
    || valid3Char
  where
    predefinedValid = ["forced", "sdh", "cc"]
    strippedExt = strip ext
    valid2Char = extLen == 2 && Set.member strippedExt iso639_1
    valid3Char = extLen == 3 && Set.member strippedExt iso639_2
    extLen = length strippedExt
    -- Remove the leading '.', if it exists:
    strip ('.' : rest) = rest
    strip noDot = noDot
