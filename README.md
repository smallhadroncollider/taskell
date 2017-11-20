# Taskell

A CLI task manager, written in Haskell

- Tasks stored in a `json` file in the current working directory - for easy version control
- Uses `vim` style key-bindings

## Controls

- `a` add a to do (`Enter`/`Esc` to stop)
- `e` edit a to do (`Enter`/`Esc` to stop)
- `Space`: mark as complete
- `j`/`Down`: move down
- `k`/`Up`: move up
- `h`/`Left`: switch list 
- `l`/`Right`: switch list 
- `q`: quit

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
- ~`h`/`l` to move left and right~
- ~save changes to `taskell.json`~
- ~create `taskell.json` if it doesn't exist~
- ~edit tasks using `e`~
- ~add tasks using `a`~

### 0.2

- add tags/labels to tasks
- filter by tag/label
- run with a filename to open any file
- 'o' open file
