{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Bindings where

import ClassyPrelude hiding (Left, Nothing, Right)

data ActionType
    = Quit
    | Undo
    | Search
    | Due
    | Help
    | Previous
    | Next
    | Left
    | Right
    | Bottom
    | New
    | NewAbove
    | NewBelow
    | Duplicate
    | Edit
    | Clear
    | Delete
    | Detail
    | DueDate
    | ClearDate
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | Complete
    | MoveMenu
    | ListNew
    | ListEdit
    | ListDelete
    | ListRight
    | ListLeft
    deriving (Show, Eq)

data BindingType
    = BChar Char
    | BKey Text
    deriving (Eq)

data Binding = Binding
    { _action      :: ActionType
    , _configKey   :: Text
    , _binding     :: [BindingType]
    , _description :: Text
    }

bindings :: [Binding]
bindings =
    [ Binding Quit "quit" [BChar 'q'] "Quits taskell"
    , Binding Undo "undo" [BChar 'u'] "Undo"
    , Binding Search "search" [BChar '/'] "Starts search"
    , Binding Due "due" [BChar '!'] "Displays tasks with due dates"
    , Binding Help "help" [BChar '?'] "Displays help"
    , Binding Previous "previous" [BChar 'k'] "Selects previous task"
    , Binding Next "next" [BChar 'j'] "Selects next task"
    , Binding Left "left" [BChar 'h'] "Selects previous list"
    , Binding Right "right" [BChar 'l'] "Selects next list"
    , Binding Bottom "bottom" [BChar 'G'] "Selects last task in list"
    , Binding New "new" [BChar 'a'] "Creates a new task"
    , Binding NewAbove "newAbove" [BChar 'O'] "Creates a new task above"
    , Binding NewBelow "newBelow" [BChar 'o'] "Creates a new task below"
    , Binding Duplicate "duplicate" [BChar '+'] "Duplicates the current task"
    , Binding Edit "edit" [BChar 'e', BChar 'A', BChar 'i'] "Edits the current task"
    , Binding Clear "clear" [BChar 'C'] "Clears the text for the current task"
    , Binding Delete "delete" [BChar 'D'] "Deletes the current task"
    , Binding Detail "detail" [BKey "Enter"] "Displays the details for the current task"
    , Binding DueDate "dueDate" [BChar '@'] "Edit the due date for the current task"
    , Binding ClearDate "clearDate" [BKey "Backspace"] "Removes the due date for the current task"
    , Binding MoveUp "moveUp" [BChar 'K'] "Moves the current task up"
    , Binding MoveDown "moveDown" [BChar 'J'] "Moves the current task down"
    , Binding MoveLeft "moveLeft" [BChar 'H'] "Moves the current task into the previous list"
    , Binding MoveRight "moveRight" [BChar 'L'] "Moves the current task into the next list"
    , Binding
          Complete
          "complete"
          [BKey "Space"]
          "Moves the current task to the last list and removes any due dates"
    , Binding MoveMenu "moveMenu" [BChar 'm'] "Displays the move menu"
    , Binding ListNew "listNew" [BChar 'N'] "Creates a new list"
    , Binding ListEdit "listEdit" [BChar 'E'] "Edits the list title"
    , Binding ListDelete "listDelete" [BChar 'X'] "Deletes the list"
    , Binding ListLeft "listLeft" [BChar '<'] "Moves the list left"
    , Binding ListRight "listRight" [BChar '>'] "Moves the list right"
    ]
