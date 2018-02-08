## Goals

- Nix support?
- apt-get repository

## Refactoring

- Break up State module
- Simplify state by composing Actions
- Use a map in Actions to tidy things up/add custom key support
- Add some tests
- Avoid having to normalise the state?

## Bugs

- Items near bottom of the list jump in position
- List titles sometimes go missing
- Up and down in search gets a bit lost

## To Do

- Left/Right arrow keys in insert mode
- If column width is more than the screen width then padding and width should be reduced so that it fits (within reason)
- Add tags/labels with `t`
- Move between lists with `m` - shows possible lists
- On `?` show keyboard commands
- Add due dates to tasks with `d`
- `.taskell` config file in home directory
- URL field - plus config to run specific command when selected (e.g. `open -a Chrome.app #{url}`)
- Copy and paste?
- Add custom key support
- Add Trello import
- Add `--convert-to-md` and `--convert-to-json` options

## Done

- `a` to add
- `e` to edit
- `Space` to mark complete
- `j`/`k`/`up`/`down` to move up/down list
- `h`/`l`/`Left`/`Right` to move between lists
- `q` to quit
- Create taskell.json if doesn't exist
- Move tasks up/down
- Delete with `D`
- No padding on lists
- Order of lists is wrong
- Can't switch lists
- Add doesn't go to bottom of list
- foldr1 in UI/Main will break if no lists
- Cursor support
- Move tasks between lists with `H`/`K`
- Move to next list with `Space`
- Create new list with `N`
- Delete lists with `X`
- Enter while in add mode creates new/Esc leaves add mode
- Rename Tasks to List
- Rename AllTasks to Lists
- Horizontal scrolling
- Scrolling long lists
- If no lists crashes on up/down - using index in AllTasks
- State should return Maybe?
- Select lists with `1-9`
- Run with any correctly formatted json file
- Wrap lines
- UI modules need refactoring
- Horizontal scrolling stops working if a word longer than the column width is entered
- Cursor no longer in the right place
- Space in edit/add mode doesn't move cursor along - bit disconcerting
- If no items in current list, returns a Nothing, so everything dissappears
- CreateList mode doesn't display anything
- Cursor doesn't show on CreateList mode
- Reordering lists with `>` and `<`
- `o` to add on line below
- `O` to add on line above
- `G` take to bottom of file
- Undo with `u`
- Should only save after adding a new item or finished editing an item - should save when pressing Enter while adding new items
- `C` to change task - i.e. deletes text and goes into edit mode
- Space moves to next and stays in list / H & L move but keep current
- Cursor position assumes single line list title
- `E` edits list title
- Pressing enter on Edit creates new task
- Reordering leftmost list left takes you to last list
- Linux binary
- Homebrew support
- Tabs in tasks throw off cursor and wrapping
- Cursor vertical offset is wrong when adding a new list
- Search using `/`
- Change foldl to foldl`
- Debian package
- Empty tasks aren't obviously selectable
- Pressing `e` on a blank list breaks things
- Add support for Markdown
- Fixed Unicode support
- Scrolling
- New list outside view doesn't scroll
- New item outside view doesn't scroll
- Vertical scrolling falls behind
- Use concurrency for IO
- Search UI
- Vertical scrolling hides list titles
- Use Brick for UI
- `C` doesn't work properly
