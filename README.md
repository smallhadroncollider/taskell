# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

## Controls

- `q`: quit
- `j`/`Down`: move down
- `k`/`Up`: move up
- `h`/`Left`: move left
- `l`/`Right`: move right
- `Space`: mark as complete

## Storage

Stores in a local `taskell.json` file:

```json
[
    {
        "completed": true,
        "description": "Do this"
    },
    {
        "completed": false,
        "description": "Do that"
    }
]
```

---

## Roadmap

### 0.1

- ~`q` to quit~
- ~`j`/`k` to move up and down~
- ~`<space>` to mark completed~
- `h`/`l` to move left and right 
- add tasks using `a`
- edit tasks using `e`
- ~save changes to `taskell.json`~
- ~create `taskell.json` if it doesn't exist~

### 0.2

- add tags/labels to tasks
- filter by tag/label
