## Misc.

- Add Twitter link to website/GitHub
- Automate website publishing when doing a new build
- Move image to taskell.app
    * Move image
    * Update site
    * Update GitHub README.md
    * Remove img branch

## Refactoring

- Update Task field naming
    * Task: description -> name/title
    * Task: summary -> description
    * UI.Modal.SubTasks -> UI.Modal.Detail
- Break up State module
- Use a map in Actions to tidy things up/add custom key support
- Avoid having to normalise the state?
- Remove duplication of config - currently using ini and hard-coded defaults
- Move Help modal creation into Template Haskell
- Use lenses for nested data?
- Add a List widget for common actions between tasks and sub-tasks
- Tidy up load functions in IO.Taskell

## Bugs

- Very long words should get hyphenated
- Help modal needs to wrap and scroll
- No obvious way to know if there are more items in a list off-screen
- Task description not visible on shorter screens - needs to have visibility if not scrolling through sub-tasks
- Modal boxes shouldn't be full height unless they need to be
- Up and down in search gets a bit lost
- Caching doesn't clear properly when using `o` and `O`
- Multiple spaces in a line don't show up as more than one, but are saved as more than one
- The isBlank check on tasks could potentially delete a task with no description but which does have sub-tasks

## Features

- GitHub checklist support - []/[x]
- Add custom key support
- Show filename somewhere
- Editable title?
- Add tags/labels with `t`
- Add due dates to tasks with `d`
- URL field - plus config to run specific command when selected (e.g. `open -a Chrome.app #{url}`)
- Should change list numbering to letters when in move list mode
- Redo functionality
- Make trello token UX better
    * Open link automatically?
    * Ask for token and save to ini file automatically
- Reordering sub-tasks
- Add Trello syncing

## In Progress

- Add Trello import
    * ~Basic trello import~
    * Add due date support
    * Add sub-tasks support
    * Add card summary support
- Task body - e.g. as well as sub lists, have a longer description
- Better Trello import errors - e.g. auth vs. parsing issues

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
- Vertical spacing doesn't work if the current item is blank
- Empty tasks - i.e. just a space - don't show up
- Editing list title doesn't always have visibility
- Left/Right arrow keys in insert mode
- Share code between tasks and sub-tasks lists?
    * ~Move wrap into Field widget~
    * ~Use Field for search~
    * ~Use Field for sub-tasks~
    * ~Use Field for titles~
    * ~Use Field for tasks~
    * ~Make sure `C` works~
- Copy and paste
    * ~List titles~
    * ~Search~
- Multiple spaces at the beginning of a line can break cursor positioning
