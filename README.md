# tvmv

A command-line tool to bulk-rename TV episode files with minimal fuss.

Integrates with [TMDB](https://www.themoviedb.org/). Uses API metadata to
automatically rename your files to something media-server-friendly. (For Plex,
Kodi, &c.)

```
$ tvmv mv -n buffy -s 7
```

That's all there is to it!


## Table of Contents

* [Quickstart / Demo](#quickstart--demo)
* [Overview](#overview)
* [Prerequisite: an API key](#prerequisite-an-api-key)
* [Installation](#installation)
  + [Binary installation](#binary-installation)
  + [Building from source](#building-from-source)
* [Using tvmv](#using-tvmv)
  + [API key location](#api-key-location)
  + [tvmv commands](#tvmv-commands)
     * [mv](#mv)
        + [mv mode 1: Autodetect the season/episode numbers](#mv-mode-1-autodetect-the-seasonepisode-numbers)
        + [mv mode 2: Specify a season number](#mv-mode-2-specify-a-season-number)
        + [Which mode should I use?](#which-mode-should-i-use)
     * [undo](#undo)
        + [Log files](#log-files)
     * [search](#search)
  + [A note on the mv command and show-matching](#a-note-on-the-mv-command-and-show-matching)
  + [Other options](#other-options)
  + [Configuration / Customization?](#configuration--customization)
* [Running tests](#running-tests)
* [FAQ](#faq)
  + [What exactly does tvmv do?](#what-exactly-does-tvmv-do)
  + [Is there a GUI?](#is-there-a-gui)
  + [How is this different from [bulk-rename tool X]?](#how-is-this-different-from-bulk-rename-tool-x)
  + [What about FileBot?](#what-about-filebot)
  + [Sonarr/Radarr?](#sonarrradarr)


## Quickstart / Demo

First download the latest release for your platform from [the releases
page](https://github.com/keithfancher/tvmv/releases). With that out of the
way...

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
"Agatha Christie's Poirot - s12e01 - Three Act Tragedy.mp4"
"Agatha Christie's Poirot - s12e02 - Hallowe'en Party.mp4"
"Agatha Christie's Poirot - s12e03 - Murder on the Orient Express.mp4"
"Agatha Christie's Poirot - s12e04 - The Clocks.mp4"
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


## Installation

You've got two options:

1. Installing a binary
   [release](https://github.com/keithfancher/tvmv/releases). The
   quick-and-easy route.
2. Building from source. Not recommended unless you need a particular
   cutting-edge feature that isn't included in the latest binary release. (But
   don't worry: building isn't *complicated*. It's just *slow*.)

More details below.

### Binary installation

If you've downloaded [a binary
release](https://github.com/keithfancher/tvmv/releases), you can simply
extract the `tvmv` executable somewhere in your `PATH`. No installation
process is required. (Other than configuring your API key, as covered in the
following section.)

To uninstall, simply remove the binary.

### Building from source

We use `stack` as our build tool. You can [install it
directly](https://docs.haskellstack.org/en/stable/install_and_upgrade/#install-stack)
or via [GHCup](https://www.haskell.org/ghcup/), whatever floats your boat.

Once you've got `stack` installed, navigate to the root of the `tvmv` project
directory (where the `package.yaml` file is located), and `stack build`:

```
$ cd tvmv
$ stack build
$ stack install
```
By default, this installs the `tvmv` binary into `~/.local/bin`. You may need
to add this directory to your `PATH`. (Or just put the binary wherever you
like. There are no ancillary files to worry about.)


## Using tvmv

The "Quickstart" section above should get you pretty far. But there are a few
other things to mention.

### API key location

[An API key](https://www.themoviedb.org/documentation/api) is required! There
are three ways to pass `tvmv` your API key:

1. Via the command line (the `-k` or `--api-key` options).
2. Via the `TMDB_API_KEY` environment variable.
3. Via a file: `$XDG_CONFIG_HOME/tvmv/tmdb-api-key`. (On most unix-like
   systems, that would resolve to: `~/.config/tvmv/tmdb-api-key`. On a Windows
   system, it would be something like
   `C:/Users/<user>/AppData/Roaming/tvmv/tmdb-api-key`.)

Important note: `tvmv` will look for your API key in those locations **in that
order**. If you need to swap out API keys for a given run or a given terminal
session, it should be easy to override on the command line or with the
environment variable, respectively.

(I personally just stick my key in `~/.config/tvmv/tmdb-api-key` and forget
about it. Whatever works for you!)

### tvmv commands

`tvmv` has three commands: `mv`, `search`, and `undo`. You must specify one of
these commands to actually *do* anything.

For help with a given command, you can use `tvmv [COMMAND] -h`. For example,
to learn more about `mv`:

```
$ tvmv mv -h
```

Let's take a closer look at each command.

#### mv

This is the Main Thing, the "mv" in "tvmv". Renames (moves) your files. Users
of any Unix-like OS will recognize the name.

Here are some basic examples to get you started:

```
# Rename all files in current directory, using data for "Buffy", season 4:
$ tvmv mv -n buffy -s 4

# Do the same thing, but auto-detect the season and episode number(s):
$ tvmv mv -n buffy -a

# The exact same operation again, but using Buffy's unique ID rather than a
# name query. This ID can be easily fetched with the `tvmv search` command.
$ tvmv mv -i 95 -a

# This time, let's do season 1. And we're specifying a directory instead of
# using the current working directory:
$ tvmv mv -n buffy -s 1 ~/tv/buffy/s1

# This time, globbing for specific files -- subtitles! Note that tvmv doesn't
# care whether it's renaming episodes, subtitles, or whatever else.
$ tvmv mv -n buffy -s 1 ~/tv/buffy/s1/*srt
```

Note that the `mv` command has **two modes of operation**.

1. **tvmv autodetects the season/episode numbers** using the `-a` flag, or
2. **User specifies a season number** using the `-s` flag.

Read on to understand more about the pros/cons of each mode.

##### mv mode 1: Autodetect the season/episode numbers

"Smart mode".

This is generally the most flexible, least painful option. However, **it
requires file names which already contain season and episode numbers** in a
standard format (e.g. `s03e23` or `3x23`).

If this is the case for your files, simply pass them to `tvmv` with the `-a`
flag. Here's a quick example:

```
$ ls -1 poirot/
12x4.mkv
1x1.pilot.encoded.by.MEGA.KEWL.TEAM.LOLZ.mkv
'poirot s04e03.mkv'

$ tvmv mv -n poirot -a poirot/

$ ls -1 poirot/
"Agatha Christie's Poirot - s01e01 - The Adventure of the Clapham Cook.mkv"
"Agatha Christie's Poirot - s04e03 - One, Two, Buckle My Shoe.mkv"
"Agatha Christie's Poirot - s12e04 - The Clocks.mkv"
```

Note that `tvmv` will simply ignore files which it can't parse season/episode
data from. Convenient!

##### mv mode 2: Specify a season number

"Dumb mode".

The user specifies a season number (e.g. `-s 7`) and `tvmv`, without any
processing of the filenames at all, sorts the files lexicographically, then
applies the API data to each file **in that order**.

This mode requires you to operate on only **a single season at a time**. It
also requires you to operate on **a full, contiguous season**. If you attempt
to operate on too few or too many files in this mode, `tvmv` will (by default)
throw an error. (This behavior can be changed with the `-p` flag, which allows
partial matches. Use at your own peril!)

If your files do *not* contain standard-format season/episode numbers already,
you must use this mode.

Note that you can run into trouble if your files don't (lexicographically)
sort correctly. For example, if you have files whose episode numbers aren't
zero-padded, the sorting can go wrong, e.g.:

```
ep1.mp4
ep10.mp4
ep2.mp4
```

In this case, you'll have to zero-pad the episode numbers for `tvmv` to work
correctly. (Or fix the filenames to be usable with the "auto-detect" mode.)

##### Which mode should I use?

**Do your file names contain season and episode numbers already**, in a
standard format like `s03e23` or `3x23`? If so, just use the autodetect mode
(`-a`). It's more flexible, and can operate on non-contiguous files from
arbitrary seasons.

Otherwise, you'll need to specify a season number with `-s`, and can only
operate over contiguous episodes from a single season at a time.

#### undo

The `undo` command will undo the `mv` operations that you just ran, resetting
your files. It depends on a `tvmv` log file sitting in your current directory.

```
# This will undo the most recent `mv` operation. It depends on a tvmv log file
# sitting in your current directory.
$ tvmv undo

# Same idea here, but let's specify a specific tvmv log file rather than using
# the most recent one:
$ tvmv undo tvmv-log-123456.txt
```

What are these log files, you ask? Read on!

##### Log files

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

#### search

The `search` command searches TMDB for shows that match the given text query.
Fetches name, unique ID, and a little metadata about each result. For example:

```
# Searches TMDB for shows that match the query "buffy":
$ tvmv search buffy
```

The TMDB URL for each result will also be displayed, which can be handy to
quickly grab some more info about a result or verify it's the show you expect.

### A note on the `mv` command and show-matching

As you can see in the examples above, you can match a show on *either* its
name *or* its unique ID. If you match a show by name (using the `-n` flag of
the `mv` command), `tvmv` will use the **first** match returned by the API for
your query. This is the same order that's returned by `tvmv search`, so you
can preview the results if you like. (And you will have a chance to confirm
the rename before it happens, of course.)

Luckily, TMDB's search is generally very sane. If you search for `buffy`, you
get back the right "Buffy" as the first result. Fragments are fine, too -- if
you search `thrones`, you get back "Game of Thrones". If you search `simps`,
you get back "The Simpsons", etc.

If you don't trust this behavior, or if you prefer something guaranteed to be
repeatable (or scriptable?), you can match a show by its unique ID using the
`-i` flag. As mentioned above, you can get a show's id via `tvmv search`.

### Other options

By default, `tvmv` will ask for user confirmation before making any file
changes. (This goes for both the `mv` and `undo` commands.) If you want to
skip this confirmation step, simply use the `-f` (or `--force`) command-line
option. (For example, `tvmv undo -f`.)

### Configuration / Customization?

There isn't any! I'm banking on sane defaults here. If there's a demand, say,
for episode name templates or something, I'll think about adding them in. But
for now, it just does its thing.

That said: let me know if it doesn't work for your use-case! I built this
mostly with my own needs in mind.


## Running tests

There are two test suites: `unit` and `integration`. The following will run
them both, which is the default:

```
$ stack test
```

Or you can run either one individually:

```
$ stack test :unit
$ stack test :integration
```

The unit tests are pure and isolated, as you'd expect. The integration tests
will create and rename actual files on your actual file system (and then clean
up when they're done, of course).

Note that, despite being called "integration tests", they do *not* connect to
any real external API (and of course no API key is required). The
"integration" in this case is with the filesystem.

There are currently no (automated) full end-to-end tests, but I provided a bit
of test data to play with, e.g.:

```
$ stack build
$ stack exec tvmv -- mv -n poirot -s 12 test/data/
$ stack exec tvmv -- undo
```
Of course since these are real calls to the application, you *will* need an
API key for this.


## FAQ

### What exactly does tvmv do?

It fetches TV episode metadata from an API, then uses that metadata to
automatically rename TV episode files into a media-server-friendly (and
human-friendly) format.

### Is there a GUI?

Nope, it's a command-line tool. It probably wouldn't be hard to build a GUI on
top of it, but that's not a priority for me at the moment. (Calling `tvmv mv
-n buffy -a` is quick and easy! That's really all there is to it.)

### How is this different from \[bulk-rename tool X\]?

Many folks have suggested tools like PowerRename and Bulk Rename Utility as
alternatives. Here's why these tools are different:

1. These are generic, pattern-based bulk-rename tools. They're great! But they
   don't automatically pull TV episode metadata from an API, which is what
   `tvmv` does.
2. Most of these tools are Windows-only. (I don't use Windows.)

### What about FileBot?

FileBot is super-cool. But it has a *ton* of features I don't use, need, or
want, and the functionality that I *do* want (pulling data from an API and
renaming files) isn't quite worth the price tag for me.

Also, I don't need a JRE to use `tvmv` ;)

### Sonarr/Radarr?

These tools are fantastic, and I highly recommend them if you need/want the
functionality they provide. But they are *way* overkill for my needs. To
install software like this simply to pull API data and rename files is... too
much! (For me, at least.)
