## Goals

- Nix support?
- apt-get repository

## Refactoring

- Break up State module
- Simplify state by composing Actions
- Use a map in Actions to tidy things up/add custom key support
- Add some tests
- Avoid having to normalise the state?
- Use Reader monad to pass around config
- Remove duplication of config - currently using ini and hard-coded
- Move Help modal creation into Template Haskell
- Use lenses for nested data?
- Share code between tasks and sub-tasks lists?

## Bugs

- Editing list title doesn't always have visibility
- Vertical spacing doesn't work if the current item is blank
- Empty tasks - i.e. just a space - don't show up
- Help modal needs to scroll on smaller windows
- No obvious way to know if there are more items in a list off-screen
- Modal boxes shouldn't be full height unless they need to be
- Up and down in search gets a bit lost

## To Do

- Left/Right arrow keys in insert mode
- Add custom key support
- Task body - e.g. as well as sub lists, have a longer description
- Show filename somewhere
- Editable title?
- Add tags/labels with `t`
- Add due dates to tasks with `d`
- URL field - plus config to run specific command when selected (e.g. `open -a Chrome.app #{url}`)
- Should change list numbering to letters when in move list mode
- If column width is more than the screen width then padding and width should be reduced so that it fits (within reason)
- Add Trello import
- Copy and paste
    * List titles
    * Search

## In Progress


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
- Custom colours
- `.taskell` config file in home directory
- Rename Persistence to IO.Taskell
- List titles sometimes go missing
- Use Template Haskell to import in config file templates
- On `?` show keyboard commands
- Remove size from state
- Sub-lists
    * ~Scrolling in sub-tasks~
    * ~Press Enter to create next~
    * ~Word wrapping~
    * ~Searching~
    * ~Delete items~
- No cursor in sub-task view
    * ~Single line~
    * ~Multi-line~
- Customisable Markdown format
    * ~Change top level headers~
    * ~Change top level list item: e.g. to H3 instead of li~
    * ~Change sub-list: e.g. from "    *" to "-"~
- Feels sluggish in sub-task view - cache main view?
- Leaving search only refreshes current list
- Display a warning if any line of the file could not be parsed - otherwise could lead to data loss
- One bad config line stops all config from working - needs to merge with defaultConfig
- Split up Draw/Modal code into more logical chunks
- Move between lists with `m` - shows possible lists
- Caching issue when using `m` to move lists - doesn't update previous list
- Copy and paste?
- Pressing Enter on empty list shows an subtasks box with an error
- Cursor goes missing on the left hand side at the end of a line - needs to wrap
- Sub-task count not visible on last item in a list longer than the vertical height
