{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.Text as T (justifyRight)

import Events.Actions.Types as A (ActionType (..))
import IO.Keyboard.Types    (Bindings, bindingsToText)

import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

descriptions :: [([ActionType], Text)]
descriptions =
    [ ([A.Help], "Show this list of controls")
    , ([A.Previous, A.Next, A.Left, A.Right], "Move down / up / left / right")
    , ([A.Bottom], "Go to bottom of list")
    , ([A.New], "Add a task")
    , ([A.NewAbove, A.NewBelow], "Add a task above / below")
    , ([A.Edit], "Edit a task")
    , ([A.Clear], "Change task")
    , ([A.Detail], "Show task details / Edit task description")
    , ([A.DueDate], "Add/edit due date (yyyy-mm-dd)")
    , ([A.MoveUp, A.MoveDown], "Shift task down / up")
    , ([A.MoveLeft, A.MoveRight], "Shift task left / right")
    , ([A.MoveMenu], "Move task to specific list")
    , ([A.Delete], "Delete task")
    , ([A.Undo], "Undo")
    , ([A.ListNew], "New list")
    , ([A.ListEdit], "Edit list title")
    , ([A.ListDelete], "Delete list")
    , ([A.ListLeft, A.ListRight], "Move list left / right")
    , ([A.Search], "Search")
    , ([A.Quit], "Quit")
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
