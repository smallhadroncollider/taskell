{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.Actions.Types where

import ClassyPrelude

data ActionType
    = AQuit
    | AUndo
    | ASearch
    | AHelp
    | APrevious
    | ANext
    | ALeft
    | ARight
    | ABottom
    | ANew
    | ANewAbove
    | ANewBelow
    | AEdit
    | AClear
    | ADelete
    | ADetail
    | ADueDate
    | AMoveUp
    | AMoveDown
    | AMoveLeft
    | AMoveRight
    | AMoveMenu
    | AListNew
    | AListEdit
    | AListDelete
    | AListRight
    | AListLeft
    | ANothing
    deriving (Show, Eq, Ord, Enum)

all :: [ActionType]
all = [toEnum 0 ..]

read :: Text -> ActionType
read "quit"       = AQuit
read "undo"       = AUndo
read "search"     = ASearch
read "help"       = AHelp
read "previous"   = APrevious
read "next"       = ANext
read "left"       = ALeft
read "right"      = ARight
read "bottom"     = ABottom
read "new"        = ANew
read "newAbove"   = ANewAbove
read "newBelow"   = ANewBelow
read "edit"       = AEdit
read "clear"      = AClear
read "delete"     = ADelete
read "detail"     = ADetail
read "dueDate"    = ADueDate
read "moveUp"     = AMoveUp
read "moveDown"   = AMoveDown
read "moveLeft"   = AMoveLeft
read "moveRight"  = AMoveRight
read "moveMenu"   = AMoveMenu
read "listNew"    = AListNew
read "listEdit"   = AListEdit
read "listDelete" = AListDelete
read "listRight"  = AListRight
read "listLeft"   = AListLeft
read _            = ANothing
