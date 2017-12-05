# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

![Demo](https://github.com/smallhadroncollider/taskell/blob/img/demo.gif?raw=true)

## Setup

Built with [Stack](https://docs.haskellstack.org/en/stable/README/)

- `stack install && stack build`
- `stack exec taskell-exe`

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
