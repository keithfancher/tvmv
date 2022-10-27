Three commands: `mv`, `search`, and `undo`. Something that might look like
this, at least for the first full-featured version:

```
# API key in a file or env var, or OPTIONALLY can pass via CLI

# Search the API for a show, will return minimal data. Most importantly, the
# ID required to fetch more data! Will return several matches, probably.
tvmv search buffy

# Simplest use-case. Rename files in the current directory using metadata from
# the FIRST match for the show-name search "buffy", season 4.
tvmv mv -n buffy -s 4

# If you have the show's unique ID, can be even more specific. Also, here
# we're passing in a directory rather than using current dir.
tvmv mv -id abc123 -s 4 ~/tv/buffy/s4

# Will look for a tvmv log file in the current directory, and if it finds
# exactly one, will revert all logged rename operations.
tvmv undo

# Same as above, but can specify a log file.
tvmv undo ~/logz/tvmv-log-123456

```

Note that ANY file operation should pause, show what will be changed, and wait
for user confirmation before proceeding. (With an optional `-f` flag to force
it, e.g.)

Other flags:
* Do NOT write a log file (will write by default).
* Dry run (print what would have been renamed, but don't rename).

Other ideas:
* Can attempt to suss out the name of show and/or season number from the
  directory structure, make it even simpler (i.e. just `tvmv`).
* Operate over many directories, or arbitrary sets of files, maybe even piped
  in from STDIN.
* Some kind of filtering (or just let the shell filter and do something like
  above, piped-in files).
