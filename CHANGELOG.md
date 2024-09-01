# Changelog for `tvmv`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Unreleased

### Added

- A default TMDB API key. Users are no longer *required* to register their own
  API keys to use `tvmv`! This makes for a much smoother out-of-the-box
  experience. (Users can still opt to use their own API keys, same as before:
  with a file, CLI arg, or environment variable.)

### Changed

- Remove `--portable-filenames` option (aka `-w`). **Portable filenames are
  now on by default.** Users who wish to keep Unicode filenames can use the
  new `--unicode-filenames` option (aka `-u`). This option is the same as the
  previous default behavior.
- License update: `BSD-3-Clause` to `GPL-3.0-or-later`. Maybe this should go
  in the "Fixed" section ;)


## 0.5.0 - 2024-02-29

### Added

- Support multi-language subtitle files! Following the convention [defined by
  Plex](https://support.plex.tv/articles/200471133-adding-local-subtitles-to-your-media/#toc-3),
  `tvmv` will properly detect language-code metadata in filenames and ensure
  it's preserved when renaming a file. See the `README` for more details.
- More user-friendly messaging, in both success and error cases.
- Add a bit of color to the output, in the name of readability.

### Fixed

- Do not allow rename operations to overwrite existing files.
- Explicitly fail to parse multi-episode files (e.g. `Adventure Time -
  S06E01-E02.mp4`) rather than incorrectly parsing out only the *beginning* of
  the episode range.


## 0.4.0 - 2024-02-06

### Added

- CLI option to rename files more portably (`-w` or `--portable-filenames`).
  This results in output filenames which, in particular, are more
  Windows-friendly thanks to a reduced set of characters. (**NOTE:** This will
  likely become the default in a future version of `tvmv`.)
- When using auto-detect mode, can now parse file names in the `EPxx` format.
  One often sees this format with shows that only have a single season. (e.g.
  `Cowboy Bebop - EP09 - Jamming with Edward.mkv`)


## 0.3.0 - 2024-01-11

### Added

- Parsing/auto-detection of season/episode numbers with the new
  `--auto-detect` (or `-a`) CLI flag. Instead of specifying a season number
  with `-s`, use the `-a` option to automatically parse that data from the
  input filenames. This does away with many annoying limitations in previous
  versions of `tvmv`. (The need to operate on an entire season at once, for
  example.) See the `README` for more details!

### Fixed

- A single file parameter is no longer *assumed* to be a directory -- we now
  actually *check* whether it's a directory. In other words, you can now pass
  a single (non-directory) file into `tvmv` without it complaining. (This is
  most useful when using the new `--auto-detect` flag.)


## 0.2.0 - 2023-09-08

### Added

- Show a TMDB URL for each returned TV show in the `search` results list.
  Handy to find more info about a show, verify you've got the right ID, etc.

### Changed

- Use the `s##e##` episode-naming convention rather than `#x##`. (e.g.
  `s01e04` rather than `1x04`.) Some version of this is recommended by both
  [Plex](https://support.plex.tv/articles/naming-and-organizing-your-tv-show-files/#toc-0)
  and
  [Kodi](https://kodi.wiki/view/Naming_video_files/Episodes#Single_Episode_Files).
- Filter directories and `tvmv` log files from the input file list when
  running a `mv` operation. This makes it possible to run `tvmv` multiple
  times without the resulting log files causing `Mismatched number of
  episodes...` errors. Also makes it possible to run `tvmv` in a directory
  which contains video files *and* subdirectories, without having to glob
  specifically for the video files.
- Show more of each TV show description in `search` output. One line was
  simply too short to be useful.


## 0.1.0 - 2023-01-19

### Added

- Everything! First complete release. See `README.md` for more details about
  its various features, &c.
