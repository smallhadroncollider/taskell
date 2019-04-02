{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.Text as T (justifyRight)

import Events.Actions.Types (ActionType (..))
import IO.Keyboard.Types    (Bindings, bindingsToText)

import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

descriptions :: [([ActionType], Text)]
descriptions =
    [ ([AHelp], "Show this list of controls")
    , ([APrevious, ANext, ALeft, ARight], "Move down/up/left/right")
    , ([ABottom], "Go to bottom of list")
    , ([ANew], "Add a task")
    , ([ANewAbove, ANewBelow], "Add a task above/below")
    , ([AEdit], "Edit a task")
    , ([AClear], "Change task")
    , ([ADetail], "Show task details / Edit task description")
    , ([ADueDate], "Add/edit due date (yyyy-mm-dd)")
    , ([AMoveUp, AMoveDown], "Shift task down/up")
    , ([AMoveLeft, AMoveRight], "Shift task left/right")
    , ([AMoveMenu], "Move task to specific list")
    , ([ADelete], "Delete task")
    , ([AUndo], "Undo")
    , ([AListNew], "New list")
    , ([AListEdit], "Edit list title")
    , ([AListDelete], "Delete list")
    , ([AListLeft, AListRight], "Move list left/right")
    , ([ASearch], "Search")
    , ([AQuit], "Quit")
    ]

generate :: Bindings -> [([Text], Text)]
generate bindings = first (intercalate ", " . bindingsToText bindings <$>) <$> descriptions

format :: ([Text], Text) -> (Text, Text)
format = first (intercalate " / ")

line :: Int -> (Text, Text) -> Widget ResourceName
line m (l, r) = left <+> right
  where
    left = padRight (Pad 2) . withAttr taskCurrentAttr . txt $ justifyRight m ' ' l
    right = textField r

help :: Bindings -> (Text, Widget ResourceName)
help bindings = ("Controls", w)
  where
    ls = format <$> generate bindings
    m = foldl' max 0 $ length . fst <$> ls
    w = vBox $ line m <$> ls
