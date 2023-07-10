# passman
A personal password manager written in Gerbil.

## The passman model

Passman is written as a command line tool, that can be both used
directly or integrated to other front end programs

The data model consists of an encrypted vault file, which contains a
list of entries.  Each entry is a key-value pair, that can hold
arbitrary sensitive information.  The `passman` cli provides commands
to create, read, and update vaults.

Note that passman is intended as a personal user's tool, it does not
attempt to be a database for multiple concurrent users, although that
is certainly possible programmatically through a front end.

## Installation
You can install it through the gerbil package manager:

```
gxpkg install github.com/vyzo/passman
```

Note: passman requires a recent gerbil master build, for scrypt support.

## Static build using Docker
You can also install via Docker removing the need for installing Gambit/Gerbil

``` sh
make
```
This will generate a passman-bin which is statically linked.

``` sh
make install
```
This will install it to /usr/local/bin

## Usage

```
$ passman help
Usage: passman  <command> command-arg ...

Commands:
 create                           create a new vault
 add                              add new entries to the vault
 update                           update an existing entry in the vault
 get                              get an entry from the vault
 search                           search for an entry using regular expressions; the search examins the value of all fields
 delete                           delete an entry from the vault
 dump                             dump the contents of the vault
 generate                         generate a random password with common rules
 help                             display help; help <command> for command help
```

### Creating a new vault

```
$ passman help create
Usage: passman create [command-option ...]
       create a new vault

Command Options:
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]
```

### Adding new entries to a vault

```
$ passman help add
Usage: passman add [command-option ...] <key>
       add new entries to the vault

Command Options:
 -j --json                        output in json
  --input <input>                 specify input non interactively [default: #f]
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]

Arguments:
 key                              the entry's key, if not -. If it is -, then multiple entries can be processed, but they must have an explicit key argument
```

### Updating existing entries
```
$ passman help update
Usage: passman update [command-option ...] <key>
       update an existing entry in the vault

Command Options:
 -j --json                        output in json
  --input <input>                 specify input non interactively [default: #f]
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]

Arguments:
 key                              the entry's key, if not -. If it is -, then multiple entries can be processed, but they must have an explicit key argument
```

### Retrieving entries
```
$ passman help get
Usage: passman get [command-option ...] <key>
       get an entry from the vault

Command Options:
 -j --json                        output in json
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]

Arguments:
 key                              the entry's key
```

### Searching for entries
```
$ passman help search
Usage: passman search [command-option ...] <niddle>
       search for an entry using regular expressions; the search examines the value of all fields

Command Options:
 -j --json                        output in json
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]
 -r                               the niddle is a regular expression

Arguments:
 niddle                           the value to match; can optionally be a regex
```

### Dumping a vault
```
$ passman help dump
Usage: passman dump [command-option ...]
       dump the contents of the vault

Command Options:
 -p --path <path>                 path to the vault file [default: /home/vyzo/.passman/default.vault]
  --passphrase <passphrase>       specify the passphrase in the command line instead of asking for it [default: #f]
 -j --json                        output in json
 -y --yes                         don't ask for confirmation, just do it!
```

### Generating passwords
```
$ passman help generate
Usage: passman generate [command-option ...]
       generate a random password with common rules

Command Options:
 -l  <lowercase>                  minimum number of lowercase chars [default: 1]
 -u  <uppercase>                  minimum number of uppercase chars [default: 1]
 -d  <digit>                      minimum number of digits [default: 1]
 -s  <special>                    minimum number of special chars [default: 1]
 -c  <length>                     password length [default: 12]
```

# Wishlist

These are some features that could make passman a more broadly usable
tool, escaping from vyzo's personal use. Feel free to experiment, prs
are always welcome.

## Cloud sync

Since passman uses a flat encrypted vault file, it is straightforward
to use a tool like `rclone` and manually sync in the command
line. Regardless it would be nice to integrate it as a passman command
and provide a better user experience.

## Graphical frontend

A graphical frontend, for integration with the desktop environment would be quite neat.
That would make it easier for users to fetch their password for web browsing and other user interfaces that require credentials.

A note: why prefer passman over (say) firefox's password manager?
Simple, it is _encrypted_, while the browser's vault is not.

## Android/iOS App

That would be the killer feature for me, together with sync. It would
allow users to have a one stop password manager for all their personal uses.

In principle, it is possible to cross-compile passman for Android/iOS,
albeit with a bit of work.  We would like to improve the gerbil
toolchain so as cross compilation is a breeze.  You can also build
passman directly on the device (for instance with termux), but clearly
this is neither a nice developer and user experience.

# Copyright
© 2023 vyzo; License: MIT
