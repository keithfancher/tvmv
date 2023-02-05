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
  + [Building from source](#building-from-source)
  + [Binary installation](#binary-installation)
* [Using tvmv](#using-tvmv)
  + [API key location](#api-key-location)
  + [tvmv commands](#tvmv-commands)
  + [A note on the mv command and show-matching](#a-note-on-the-mv-command-and-show-matching)
     * [Matching the number of episodes and files](#matching-the-number-of-episodes-and-files)
  + [Log files](#log-files)
  + [Other options / Getting help](#other-options--getting-help)
  + [Configuration / Customization?](#configuration--customization)
* [Running tests](#running-tests)


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

1. Build from source. Not recommended unless you need a particular
   cutting-edge feature that isn't included in the latest binary release. (But
   don't worry: building isn't *complicated*. It's just *slow*.)
2. Installing a binary
   [release](https://github.com/keithfancher/tvmv/releases). The
   quick-and-easy route.

More details below.

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

### Binary installation

If you've downloaded [a binary
release](https://github.com/keithfancher/tvmv/releases), you can simply
extract the `tvmv` executable somewhere in your `PATH`. No installation
process is required. (Other than configuring your API key, as covered in the
following section.)

To uninstall, simply remove the binary.


## Using tvmv

The "Quickstart" section above should get you pretty far. But there are a few
other features to mention.

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

#### Matching the number of episodes and files

By default, `tvmv` will throw an error if the number of files passed in does
not match the number of episodes in a given season. This is a safety feature,
designed to keep you from shooting yourself in the foot, but might be
undesirable in certain cases.

For example, if you only have the first `n` episodes of a season, but still
want to easily rename them. Or if the TMDB data includes "extra" stuff at the
end of a season. In these cases, you can pass the `--allow-partial` flag to
the `mv` command (aka `-p`) and the error-checking will become less strict.

(Note that, even with this flag, `tvmv` will still throw an error if you have
more *files* than episodes. In this case, you can simply glob for the files
you want to rename or put them in a directory. It doesn't quite make sense to
pass more files than there are episodes to `tvmv`.)

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
