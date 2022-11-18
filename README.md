# tvmv

Bulk-rename TV episode files with minimal fuss. Integrates with
[TMDB](https://www.themoviedb.org/).

**NOTE:** `tvmv` is feature-complete and works great, but I'm still polishing
up its first release. I would consider it *beta* software.

## Quickstart / Demo

```
# You'll need a (free!) TheMovieDB API key. Pass it in via the CLI, an env
# var, or a file. We'll use an env var for this example:
$ export TMDB_API_KEY="ABCD1234"

# Now, let's check out a season of a show we ripped:
$ cd ~/rips/poirot/season12

# Who named these files? Super-annoying :(
$ ls -1
'Ep1.mp4'
'Ep2.mp4'
'Ep3.mp4'
'Ep4.mp4'

# One command later...
$ tvmv mv -n poirot -s 12

# ...ahh, much better!
$ ls -1
"Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4"
"Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4"
"Agatha Christie's Poirot - 12x03 - Murder on the Orient Express.mp4"
"Agatha Christie's Poirot - 12x04 - The Clocks.mp4"
```

## Overview

tvmv is a minimal, (almost-)zero-config command-line tool to bulk-rename your
ripped or (legally!) downloaded TV episode files. It has sane defaults, talks
to a sane API, and produces useful filenames that are compatible with Plex,
Kodi, and most other media servers.

## Prerequisite: an API key

You will need [an API key from
TMDB](https://www.themoviedb.org/documentation/api). Sorry for the hassle! It
only takes a minute to sign up and **it's free**. Feel free to put `tvmv` as
the app-in-question in the sign-up form if it asks. (This is why I say
"minimal fuss" and not "no fuss".)

## Build requirements

We use `stack` as our build tool. You can [install it
directly](https://docs.haskellstack.org/en/stable/install_and_upgrade/#install-stack)
or via [GHCup](https://www.haskell.org/ghcup/), whatever floats your boat.

Once you've got `stack` installed, it takes care of the rest:

```
$ stack build
$ stack install
```
By default, this installs the `tvmv` binary into `~/.local/bin`. You may need
to add this directory to your `PATH`. (Or just put the binary wherever you
like. There are no ancillary files to worry about.)

Note that I don't currently provide any pre-built binaries. Perhaps after I've
tagged the first official "release"...

## Using tvmv

The "Quickstart" section above should get you pretty far. But there are a few
other features to mention.

### API key location

[An API key](https://www.themoviedb.org/documentation/api) is required! There
are three ways to pass `tvmv` your API key:

1. Via the command line (the `-k` or `--api-key` options).
2. Via the `TMDB_API_KEY` environment variable.
3. Via a file: `$XDG_CONFIG_HOME/tvmv/tmdb-api-key`. (On most unix-like
   systems, that would resolve to: `~/.config/tvmv/tmdb-api-key`.)

Important note: `tvmv` will look for your API key in those locations **in that
order**. If you need to swap out API keys for a given run or a given terminal
session, it should be easy to override on the command line or with the
environment variable, respectively.

(I personally just stick my key in `~/.config/tvmv/tmdb-api-key` and forget
about it. Whatever works for you!)

### tvmv commands

`tvmv` has three commands: `mv`, `search`, and `undo`. Let's look at some
examples:

```
### THE mv COMMAND:

# Rename all files in current directory, using data for "Buffy", season 4:
$ tvmv mv -n buffy -s 4

# The exact same operation, but using Buffy's unique ID rather than a name
# query. This ID can be easily fetched with the `tvmv search` command.
$ tvmv mv -i 95 -s 4

# This time, let's do season 1. And we're specifying a directory instead of
# using the current working directory:
$ tvmv mv -n buffy -s 1 ~/tv/buffy/s1

# This time, globbing for specific files -- subtitles! Note that tvmv doesn't
# care whether it's renaming episodes, subtitles, or whatever else.
$ tvmv mv -n buffy -s 1 ~/tv/buffy/s1/*srt


### THE undo COMMAND:

# This will undo the `mv` operations that you just ran, resetting your files.
# It depends on a tvmv log file sitting in your current directory.
$ tvmv undo

# Same idea here, but let's specify a specific tvmv log file rather than using
# the most recent one:
$ tvmv undo tvmv-log-123456.txt


### THE search COMMAND

# Searches TMDB for shows that match the query "buffy". Fetches name, unique
# ID, and a little bit of metadata for each result:
$ tvmv search buffy

```

### A note on the `mv` command and show-matching

As you can see in the examples above, you can match a show on *either* its
name *or* its unique ID. If you match a show by name (using the `-n` flag of
the `mv` command), `tvmv` will use the **first** match returned by the API for
your query. (You will have a chance to confirm the rename before it happens,
of course.)

Luckily, TMDB's search is generally very sane. If you search for `buffy`, you
get back the right "Buffy". Fragments are fine, too -- if you search
`thrones`, you get back "Game of Thrones". If you search `simps`, you get back
"The Simpsons", etc.

If you don't trust this behavior, or if you prefer something guaranteed to be
repeatable (or scriptable?), you can match a show by its unique ID using the
`-i` flag. As mentioned above, you can get a show's id via `tvmv search`.

**NOTE:** Currently, `tvmv` will throw an error if the number of files passed
does not match the number of episodes in a season. This is a safety feature,
designed to keep you from shooting yourself in the foot, but might be
undesirable in certain cases.

(**COMING SOON:** Option to override this behavior? Or an extra confirmation
step before proceeding, rather than an error?)

### Log files

When you do an `mv` operation, `tvmv` will (by default) write a log of any
renamed files. (This is written to the *current* directory -- not necessarily
the directory where the files live.)

The sole purpose of this log file is to facilitate the `undo` command. It just
allows you to be a little more free-and-easy about renaming files. If you're
happy with the state of your files, simply delete the log.

Note that the file paths in the log files are absolute, so `undo` will work
even if you move the log file. It will **not** work, however, if you move the
TV episode files themselves.

If you don't want to write a log when using the `mv` command, simply pass the
`--no-log` flag (aka `-x`). You will *not* be able to use the `undo` command
without a log, however.

### Other options / Getting help

By default, `tvmv` will ask for user confirmation before making any file
changes. (This goes for both the `mv` and `undo` commands.) If you want to
skip this confirmation step, simply use the `-f` (or `--force`) command-line
option. (For example, `tvmv undo -f`.)

For help with a given `tvmv` command, you can use `tvmv [COMMAND] -h`. For
example, to learn more about `mv`:

```
$ tvmv mv -h
```

### Configuration / Customization?

There isn't any! I'm banking on sane defaults here. If there's a demand, say,
for episode name templates or something, I'll think about adding them in. But
for now, it just does its thing.

That said: let me know if it doesn't work for your use-case! I built this
mostly with my own needs in mind.
