# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

![Demo](https://github.com/smallhadroncollider/taskell/blob/img/demo.gif?raw=true)

## Controls

- `a` add a task (`Enter`/`Esc` to stop)
- `e` edit a task (`Enter`/`Esc` to stop)
- `j`: move down
- `k`: move up
- `h`: move left 
- `l`: move right
- `1`-`9`: select list
- `J`: shift task down
- `K`: shift task up
- `H`: shift task left 
- `L`/`Space`: shift task right
- `D`: delete task
- `N`: new list
- `X`: delete list
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
