# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a Markdown file in the current working directory - for easy version control
- Uses `vim` style key-bindings

![Demo](https://github.com/smallhadroncollider/taskell/blob/img/demo.gif?raw=true)

## Installation

### Homebrew (Mac)

You can install Taskell on your Mac using [Homebrew](https://brew.sh):

```bash
brew install smallhadroncollider/taskell/taskell
```

There are bottles (binaries) available for High Sierra and Sierra. If these are not available for your computer, homebrew will build Taskell from scratch using [Stack](https://docs.haskellstack.org/), which can take a while, particularly on older machines.

### Debian/Ubuntu

[A `.deb` package is available for Debian/Ubuntu](https://github.com/smallhadroncollider/taskell/releases). Download it and install with `dpkg -i <package-name>`.

### Fedora
Run `sudo dnf install ncurses-compat-libs` then download and run binary as described below.

### Binaries

[A binary is available for Mac and Linux](https://github.com/smallhadroncollider/taskell/releases). Download it and copy it to a directory in your `$PATH` (e.g. `/usr/local/bin` or `/usr/bin`).

## Running

- `taskell`: will use `taskell.md` in the cwd - offers to create if not found
- `taskell filename.md`: will use `filename.md` in the cwd - offers to create if not found

Taskell also supports `.json` files: `taskell filename.json`

## Controls

- `a`: add a task to bottom (`Enter`/`Esc` to stop)
- `o`: add a task below
- `O`: add a task above
- `e`/`i`/`A`: edit a task (`Enter`/`Esc` to stop)
- `C`: change task
- `j`: move down
- `k`: move up
- `h`: move left
- `l`: move right
- `G`: go to bottom of list
- `1`-`9`: select list
- `J`: shift task down
- `K`: shift task up
- `H`: shift task left (current selection follows task)
- `L`: shift task right (current selection follows task)
- `Space`: shift task right (current selection stays put)
- `D`: delete task
- `u`: undo
- `N`: new list
- `E`: edit list title (`Enter`/`Esc` to stop)
- `X`: delete list
- `<`/`>`: move list left/right
- `/`: search (`Enter` to navigate, `Esc` to leave)
- `q`: quit

### Tips

- If you're using a simple two-column "Todo" and "Done" then use the space bar to mark an item as complete while staying in the "Todo" list. If you're using a more complicated column setup then you will want to use `H`/`L` to move tasks between columns.

## Storage

Stores in a local `taskell.md` file:

```md
## To Do

- Do this

## Done

- Do That
```

Taskell also support `.json` files

---

## Roadmap

See [roadmap.md](https://github.com/smallhadroncollider/taskell/blob/develop/roadmap.md) for planned features
