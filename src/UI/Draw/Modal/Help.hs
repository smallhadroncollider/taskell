{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.Text as T (justifyRight)

import Events.Actions.Types as A (ActionType (..))
import IO.Keyboard.Types    (bindingsToText)

import IO.Keyboard.Types (Bindings)
import UI.Draw.Field     (textField)
import UI.Draw.Types     (DrawState (dsBindings), ModalWidget, TWidget)
import UI.Theme          (taskCurrentAttr)

descriptions :: [([ActionType], Text)]
descriptions =
    [ ([A.Help], "Show this list of controls")
    , ([A.Due], "Show tasks with due dates")
    , ([A.Previous, A.Next, A.Left, A.Right], "Move down / up / left / right")
    , ([A.Bottom], "Go to bottom of list")
    , ([A.New], "Add a task")
    , ([A.NewAbove, A.NewBelow], "Add a task above / below")
    , ([A.Duplicate], "Duplicate a task")
    , ([A.Edit], "Edit a task")
    , ([A.Clear], "Change task")
    , ([A.Detail], "Show task details / Edit task description")
    , ([A.DueDate], "Add/edit due date (yyyy-mm-dd)")
    , ([A.MoveUp, A.MoveDown], "Shift task down / up")
    , ([A.MoveLeft, A.MoveRight], "Shift task left / right")
    , ([A.Complete], "Move task to last list and remove any due dates")
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

line :: Int -> (Text, Text) -> TWidget
line m (l, r) = left <+> right
  where
    left = padRight (Pad 2) . withAttr taskCurrentAttr . txt $ justifyRight m ' ' l
    right = textField r

help :: ModalWidget
help = do
    bindings <- asks dsBindings
    let ls = format <$> generate bindings
    let m = foldl' max 0 $ length . fst <$> ls
    let w = vBox $ line m <$> ls
    pure ("Controls", w)
