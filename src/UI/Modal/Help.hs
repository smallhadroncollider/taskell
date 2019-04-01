{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.Text as T (justifyRight)

import IO.Keyboard.Types (Bindings, bindingsToText)

import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

descriptions :: [([Text], Text)]
descriptions =
    [ (["help"], "Show this list of controls")
    , (["previous", "next", "left", "right"], "Move down/up/left/right")
    , (["bottom"], "Go to bottom of list")
    , (["new"], "Add a task")
    , (["newAbove", "newBelow"], "Add a task above/below")
    , (["edit"], "Edit a task")
    , (["clear"], "Change task")
    , (["detail"], "Show task details / Edit task description")
    , (["dueDate"], "Add/edit due date (yyyy-mm-dd)")
    , (["moveUp", "moveDown"], "Shift task down/up")
    , (["moveLeft", "moveRight"], "Shift task left/right")
    , (["moveMenu"], "Move task to specific list")
    , (["delete"], "Delete task")
    , (["undo"], "Undo")
    , (["listNew"], "New list")
    , (["listEdit"], "Edit list title")
    , (["listDelete"], "Delete list")
    , (["listLeft", "listRight"], "Move list left/right")
    , (["search"], "Search")
    , (["quit"], "Quit")
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
