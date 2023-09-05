# Changelog for `tvmv`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).


## Unreleased

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


## 0.1.0 - 2023-01-19

### Added

- Everything! First complete release. See `README.md` for more details about
  its various features, &c.
