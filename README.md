# Taskell

[![Build Status](https://travis-ci.org/smallhadroncollider/taskell.svg?branch=master)](https://travis-ci.org/smallhadroncollider/taskell)

A CLI kanban board/task manager for Mac and Linux

- Per project task lists
- `vim` style key-bindings
- Stored using Markdown
- Clean diffs for easy version control
- Support for sub-tasks and due dates
- Trello board imports
- GitHub project imports

<a href="https://www.buymeacoffee.com/shc" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" style="height: 41px !important;width: 174px !important;box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;-webkit-box-shadow: 0px 3px 2px 0px rgba(190, 190, 190, 0.5) !important;" ></a>

Follow [@taskellcli](https://twitter.com/taskellcli) on Twitter for updates

![Demo](https://taskell.app/img/demo.gif)

---

## Contents

- [Installation](#installation)
- [Using Taskell](#using-taskell)
    - [Options](#options)
    - [Storage](#storage)
    - [Importing Trello Boards](#importing-trello-boards)
    - [Importing GitHub Projects](#importing-github-projects)
- [Configuration](#configuration)
    - [Controls](#controls)
        - [Due Dates](#due-dates)
    - [Theming](#theming)
- [Roadmap](#roadmap)

## Installation

### Homebrew (Mac)

You can install Taskell on your Mac using [Homebrew](https://brew.sh):

```bash
brew install taskell
```

There are usually bottles (binaries) available. If these are not available for your computer, Homebrew will build Taskell from scratch, which can take a while, particularly on older machines. Occasionally the build fails the first time, but usually works on a second attempt.

### Debian/Ubuntu

[A `.deb` package is available for Debian/Ubuntu](https://github.com/smallhadroncollider/taskell/releases). Download it and install with `dpkg -i <package-name>`. You may also need to install the `libtinfo5` package (`sudo apt install libtinfo5`).

### Fedora

Not officially supported, but try running `sudo dnf install ncurses-compat-libs` then download and run the binary as described below. If that doesn't work you may need to build from scratch ([Cabal](#cabal)/[Stack](#stack)).

### Binaries

[A binary is available for Mac and Debian/Ubuntu](https://github.com/smallhadroncollider/taskell/releases). Download it and copy it to a directory in your `$PATH` (e.g. `/usr/local/bin` or `/usr/bin`).

### Cabal

You can install Taskell with `cabal`:

```bash
cabal install taskell
```

Make sure you run `cabal update` if you haven't run it recently.

### Stack

If none of the above options work you can build taskell using [Stack](https://docs.haskellstack.org/en/stable/README/). First [install Stack on your machine](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then clone the repo and run `stack build && stack install`: this will build taskell and then install it in `~/.local/bin` (so make sure that directory is in your `$PATH`). Building from scratch can take a long time and occasionally doesn't work the first time (if this happens try running it again).

## Using Taskell

- `taskell`: will use `taskell.md` in the pwd - offers to create if not found
- `taskell filename.md`: will use `filename.md` in the pwd - offers to create if not found

### Options

- `-h`: show help
- `-v`: show version number
- `-t <trello-board-id>`: import a Trello board ([see below](#importing-trello-boards))
- `-g <github-project-id>`: import a GitHub project ([see below](#importing-github-projects))

#### Tips

- If you're using a simple two-column "To Do" and "Done" then use the space bar to mark an item as complete while staying in the "To Do" list. If you're using a more complicated column setup then you will want to use `H`/`L` to move tasks between columns.

### Storage

By default stores in a `taskell.md` file in the working directory:

```md
## To Do

- Do this

## Done

- Do That
```

### Importing Trello Boards

Taskell includes the ability to fetch a Trello board and store it as local taskell file.

#### Authentication

Before fetching a Trello board, you'll need to create an access token and store it in `~/.taskell/config.ini`.

- First, [get a Trello token](https://trello.com/1/authorize?expiration=never&name=taskell&scope=read&response_type=token&key=80dbcf6f88f62cc5639774e13342c20b)
- Then add it to `~/.taskell/config.ini`:

    ```ini
    [trello]
    token = <your-trello-access-token>
    ```

You can revoke access tokens [on Trello](https://trello.com/my/account)

#### Fetching

Running the following would pull down the Trello board with the ID "TRe1l0iD" into a file named `trello.md` and then open taskell with that file.

```bash
taskell -t TRe1l0iD trello.md
```

Make sure you have permission to view the Trello board, otherwise you'll get an error.

#### Limitations

- This is a one-off procedure: it effectively imports a Trello board to taskell
- Currently imports:
    - Lists
    - Cards
    - Card descriptions
    - Card due dates
    - Card checklists (merged into one list per card)


### Importing GitHub Projects

Taskell includes the ability to fetch a GitHub project and store it as local taskell file.

#### Authentication

Before fetching a GitHub board, you'll need to create a person access token and store it in `~/.taskell/config.ini`.

- First, [get a GitHub personal access token](https://github.com/settings/tokens/new)
- Make sure to tick the `repo` scope
- Then add it to `~/.taskell/config.ini`:

    ```ini
    [github]
    token = <your-github-personal-access-token>
    ```

You can delete personal access tokens [on GitHub](https://github.com/settings/tokens/)

#### Fetching

Projects can belong to [organisations](#organisations) or to [individual repositories](#repositories).

Make sure you have permission to view the GitHub project, otherwise you'll get an error.

![GitHub Demo](https://taskell.app/img/github.gif)

##### Organisations

To import a project for an organisation called "test-org" you would use the following:

```bash
taskell -g orgs/test-org github.md
```

This would then show you a list of possible projects to import. Enter the number of the project you wish to import.

##### Repositories

To import a project for the repository "test-repo" for the user "test-user":

```bash
taskell -g repos/test-user/test-repo github.md
```

This would then show you a list of possible projects to import. Enter the number of the project you with to import.

#### Limitations

- This is a one-off procedure: it effectively imports a GitHub project to taskell
- Currently imports:
    - Columns
    - Cards



## Configuration

Taskell uses the [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html), so it will look for an `$XDG_CONFIG_HOME` environmental variable and create a directory named `taskell` inside it. If this variable is not found it will create the `taskell` directory in `~/.config/`.  (If you've been using Taskell since <= 1.3.5 then it will be in a `~/.taskell` directory, feel free to move this to the XDG directory.)

Taskell has a `config.ini` file:

```ini
[general]
; the default filename to create/look for
filename = taskell.md

[layout]
; top/bottom padding for the taskell window
padding = 1

; the width of a column
column_width = 30

; the padding of a column
; for both sides, so 3 would give a gap of 6 between two columns
column_padding = 3

; the icon to use when a task has a description
; the default icon may not display on all systems
description_indicator = "≡"

; whether to show the statusbar
statusbar = true

[markdown]
; the markdown to start a title line with
title = "##"

; the markdown to start a task line with
task = "-"

; the markdown to start a sub-task line with
subtask = "    *"

; by default times are stored in UTC to avoid diffs if you
; change locations. Set this to true if it will always be
; edited in the same timezone
localTimes = false
```

Make sure that the values in the `[markdown]` section are surrounded by **double**-quotes.

If you always use sub-tasks, an alternative setup for `[markdown]` might be:

```ini
[markdown]
title = "##"

; each task is a header
task = "###"

; subtasks are list items under the header
subtask = "-"
```

**Warning**: currently if you change your `[markdown]` settings any older files stored with different settings will not be readable.

### Controls

You can edit keyboard bindings in the `bindings.ini` config file.

The default bindings can be found in [`bindings.ini`](https://github.com/smallhadroncollider/taskell/blob/master/templates/bindings.ini).

Available special keys: `<Space>`, `<Enter>`, `<Backspace>`, `<Left>`, `<Right>`, `<Up>`, `<Down>`

On a Mac you can use the `alt` characters: e.g. `quit = œ` is equivalent to `alt+q`.

You shouldn't try to assign the `1`-`9` keys, as it will not overwrite the default behaviour.

#### Due Dates

Due dates must be input with the format `YYYY-MM-DD` or `YYYY-MM-DD HH:MM`. The date will not be accepted otherwise.

You can also pass in relative times such as `1w 2d` (for 1 week and 2 days). Valid units are:

- `s` (seconds)
- `m` (minutes)
- `h` (hours)
- `d` (days)
- `w` (weeks)

These can be used in any combination. If the time is made up only of days and/or weeks, the due date will not include a time.

By default times are stored in the Markdown file as UTC. If you would like local times (and are unlikely to open the file in lots of different timezones) then you can set `localTimes` to `true` in the `markdown` section of the [config file](#configuration). If you have this setting on and you change timezone, you'll get a diff on all your times the next time you make changes to the file.


### Theming

You can edit Taskell's colour-scheme by editing `theme.ini`:

```ini
[other]

; list title
title.fg = green

; status bar
statusBar.bg = magenta
statusBar.fg = black

; current list title
titleCurrent.fg = blue

; current task
taskCurrent.fg = magenta
```

You can also change the background and default text colour:

```ini
[default]

; the app background colour
default.bg = brightBlack

; the app text colour
default.fg = white
```

The available colours are: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, `brightBlack`, `brightRed`, `brightGreen`, `brightYellow`, `brightBlue`, `brightMagenta`, `brightCyan`, `brightWhite`, or `default`

---

## Roadmap

See [roadmap.md](https://github.com/smallhadroncollider/taskell/blob/develop/roadmap.md) for planned features

### Contributing

Please check the [roadmap.md](https://github.com/smallhadroncollider/taskell/blob/develop/roadmap.md) before adding any bugs/feature requests to Issues.

Anyone is welcome to contribute to the project, but please read through [CONTRIBUTING.md](https://github.com/smallhadroncollider/taskell/blob/master/CONTRIBUTING.md) and make sure that you agree with the [Code of Conduct](https://github.com/smallhadroncollider/taskell/blob/master/CODE_OF_CONDUCT.md) before getting involved.

---

## Acknowledgements

Built using [Brick](https://github.com/jtdaugherty/brick). Thanks to [Jonathan Daugherty](https://github.com/jtdaugherty) for answering all my questions and pointing me in the right direction. Also thanks to [Jack Leigh](https://github.com/leighman) and [Thom Wright](https://github.com/ThomWright) for helping me get started. Also thanks to [Katja Durrani](https://github.com/katjad) for submitting Taskell to [`homebrew-core`](https://github.com/Homebrew/homebrew-core).
