# Changelog for `tvmv`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Unreleased

### Fixed

- Do not allow rename operations to overwrite existing files.


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
