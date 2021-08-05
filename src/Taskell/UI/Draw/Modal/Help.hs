module Taskell.UI.Draw.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.Text as T (justifyRight)

import Taskell.Events.Actions.Types as A (ActionType (..))

import Taskell.IO.Keyboard.Types (Bindings, bindingsToText)
import Taskell.UI.Draw.Field     (textField)
import Taskell.UI.Draw.Types     (DrawState (dsBindings, dsState), ModalWidget, TWidget)
import Taskell.UI.Theme          (taskCurrentAttr)
import Control.Lens ((^.))
import Taskell.Events.State.Types (mode)
import qualified Taskell.Events.State.Types.Mode as M
import Taskell.Data.Utility (updateLast)

descriptions :: [([ActionType], Text)]
descriptions =
    [ ([A.Help], "Show this list of controls")
    , ([A.Due], "Show tasks with due dates")
    , ([A.Previous, A.Next, A.Left, A.Right], "Move down / up / left / right")
    , ([A.Bottom], "Go to bottom of list")
    , ([A.Top], "Go to top of list")
    , ([A.New], "Add a task")
    , ([A.NewAbove, A.NewBelow], "Add a task above / below")
    , ([A.Duplicate], "Duplicate a task")
    , ([A.Edit], "Edit a task")
    , ([A.Clear], "Change task")
    , ([A.Detail], "Show task details / Edit task description")
    , ([A.DueDate], "Add/edit due date (yyyy-mm-dd)")
    , ([A.ClearDate], "Removes due date")
    , ([A.MoveUp, A.MoveDown], "Shift task down / up")
    , ([A.MoveLeftBottom, A.MoveRightBottom], "Shift task left / right (to bottom of list)")
    , ([A.MoveLeftTop, A.MoveRightTop], "Shift task left / right (to top of list)")
    , ( [A.Complete]
      , "Move task to the bottom of last list and remove any due dates / Mark subtask as (in)complete")
    , ([A.CompleteToTop]
      , "Move task to the top of last list and remove any due dates / Mark subtask as (in)complete")
    , ([A.MoveMenu], "Move task to specific list")
    , ([A.Delete], "Delete task")
    , ([A.Undo], "Undo")
    , ([A.Redo], "Redo")
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

help :: M.HelpScrollPosition -> ModalWidget
help s = do
    bindings <- asks dsBindings
    let ls = format <$> generate bindings
    let m = foldl' max 0 $ length . fst <$> ls
    let w = vBox $ applyScroll s $ line m <$> ls
    pure ("Controls", w)

applyScroll :: M.HelpScrollPosition -> [TWidget] -> [TWidget]
applyScroll _ [] = []
applyScroll sp (x:xs) =
  case sp of
    M.Top ->
      visible x : xs
    M.Bottom ->
      updateLast visible xs
