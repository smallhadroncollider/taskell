# Taskell

A CLI task manager, written in Haskell

## Controls

- `q`: quit
- `j`/`Down`: move down
- `k`/`Up`: move up
- `Space`: mark as complete (currently not persisted)

## Storage

Stores in a `taskell.json` file:

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

## Plan

### Controls

- `a`: add todo
- `e`: edit todo
- `h`: hide/show completed
