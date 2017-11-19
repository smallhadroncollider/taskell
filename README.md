# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

## Controls

- `q`: quit
- `j`/`Down`: move down
- `k`/`Up`: move up
- `Space`: mark as complete (currently not persisted)

## Storage

Stores in a local `taskell.json` file:

```json
[
    {
        "description": "Do this",
        "completed": true,
    },
    {
        "description": "Do this",
        "completed": false,
    }
]
```

---

## Roadmap

### 0.1

- ~`q` to quit~
- ~`j`/`k` to move up and down~
- ~`<space>` to mark completed~
- add tasks using `a`
- edit tasks using `e`
- ~hide/show completed using `.`~
- save changes to `taskell.json`
- ~create `taskell.json` if it doesn't exist~

#### Known Bugs

- up/down/space all break when hiding completed

### 0.2

- add tags/labels to tasks
- filter by tag/label
