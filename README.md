# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

![Demo](https://github.com/smallhadroncollider/taskell/blob/img/demo.gif?raw=true)

## Installation

### Mac

A [binary](https://github.com/smallhadroncollider/taskell/releases/download/0.6.1/taskell-mac-64.zip) is available for Mac. Download it and copy it to a directory in your `$PATH` (e.g. `/usr/local/bin` or `/usr/bin`). Then run `taskell` to get started.

### Linux

To run on Linux you'll need to install [Stack](https://docs.haskellstack.org/en/stable/README/) first. Then download the project and run `stack install`, this should compile the project and install it in an appropriate directory.

## Running

- `taskell`: will use `taskell.json` in the cwd - offers to create if not found
- `taskell filename.json`: will use `filename.json` in the cwd - offers to create if not found

## Controls

- `a` add a task (`Enter`/`Esc` to stop)
- `o` add a task below
- `O` add a task above
- `e`/`i` edit a task (`Enter`/`Esc` to stop)
- `j`: move down
- `k`: move up
- `h`: move left 
- `l`: move right
- `G`: go to bottom of list
- `1`-`9`: select list
- `J`: shift task down
- `K`: shift task up
- `H`: shift task left 
- `L`/`Space`: shift task right
- `D`: delete task
- `u`: undo
- `N`: new list
- `X`: delete list
- `<`/`>`: move list left/right
- `q`: quit

## Storage

Stores in a local `taskell.json` file:

```json
[
    {
        "title": "To Do",
        "tasks": [
            {
                "description": "Do this"
            }
        ],
    },

    {
        "title": "Done",
        "tasks": [
            {
                "description": "Do that"
            }
        ]
    }
]
```

---

## Roadmap

See [taskell.json](https://github.com/smallhadroncollider/taskell/blob/develop/taskell.json) for planned features
