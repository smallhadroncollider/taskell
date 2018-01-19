module UI.Draw (
    draw,
    chooseCursor
) where

import Flow.State (State, Mode(..), InsertMode(..), Pointer, lists, current, mode, search, newList)
import Brick
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description)
import Data.Taskell.String (wrap)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (mapWithIndex, length)

import Config

import UI.Types (ResourceName(..))
import UI.Attr

-- draw
addCursor :: Int -> Int -> [String] -> Widget ResourceName -> Widget ResourceName
addCursor li ti d = showCursor (RNTask (li, ti)) (Location (h, v))
    where v = length d - 1
          h = length $ last d

box :: [String] -> Widget ResourceName
box d = padBottom (Pad 1) . vBox $ str <$> d

renderTask :: Pointer -> Int -> Int -> Task -> Widget ResourceName
renderTask p li ti t =
      withAttr attr
    . addCursor li ti d
    $ box d

    where d = wrap width $ description t
          attr = if (li, ti) == p then taskCurrentAttr else taskAttr

columnNumber :: Int -> String -> String
columnNumber i s = if col >= 1 && col <= 9 then show col ++ ". " ++ s else s
    where col = i + 1

renderTitle :: Pointer -> Int -> List -> Widget ResourceName
renderTitle (p, _) li l =
      withAttr attr
    . addCursor li (-1) d
    $ box d

    where d = wrap width $ columnNumber li (title l)
          attr = if p == li then titleCurrentAttr else titleAttr

renderList :: Pointer -> Int -> List -> Widget ResourceName
renderList p li l =
      padLeftRight padding
    . hLimit width
    . viewport (RNList li) Vertical
    . vBox
    . (renderTitle p li l :)
    . toList
    $ renderTask p li `Seq.mapWithIndex` tasks l

-- draw
draw :: State -> [Widget ResourceName]
draw state = [
          viewport RNLists Horizontal
        . hLimit (Seq.length ls * (width + padding * 2))
        . padTop (Pad 1)
        . hBox
        . toList
        $ renderList (current s)  `Seq.mapWithIndex` ls
    ]
    where s = normalise state
          ls = lists s

chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode s of
    Insert (CreateList _) -> showCursorNamed (RNTask (fst (current s), -1))
    Insert EditList -> showCursorNamed (RNTask (fst (current s), -1))
    Insert CreateTask -> showCursorNamed (RNTask (current s))
    Insert EditTask -> showCursorNamed (RNTask (current s))
    _ -> neverShowCursor s

    where s = normalise state

normalise :: State -> State
normalise = newList . search
