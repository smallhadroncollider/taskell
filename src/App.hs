module App (go) where

import Control.Monad (void)
import Flow.State (State, Pointer, Mode(..), InsertMode(..), lists, current, mode, continue, path)
import Brick
import Brick.Util (fg)
import Graphics.Vty (defAttr, green, blue, magenta)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description)
import Data.Taskell.String (wrap)
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex)
import Persistence.Taskell (writeFile)

import Flow.Actions (event)

import Config

data Name = ListLocation Pointer | Empty deriving (Eq, Ord)

titleAttr :: AttrName
titleAttr = attrName "title"

titleCurrentAttr :: AttrName
titleCurrentAttr = attrName "titleCurrent"

taskCurrentAttr :: AttrName
taskCurrentAttr = attrName "taskCurrent"

taskAttr :: AttrName
taskAttr = attrName "task"

-- draw
addCursor :: Int -> Int -> [String] -> Widget Name -> Widget Name
addCursor li ti d = showCursor (ListLocation (li, ti)) (Location (h, v))
    where v = length d - 1
          h = length $ last d

box :: [String] -> Widget Name
box d = padBottom (Pad 1) . vBox $ str <$> d

renderTask :: Pointer -> Int -> Int -> Task -> Widget Name
renderTask p li ti t =
      withAttr attr
    . addCursor li ti d
    $ box d

    where d = wrap width $ description t
          attr = if (li, ti) == p then taskCurrentAttr else taskAttr

columnNumber :: Int -> String -> String
columnNumber i s = if col >= 1 && col <= 9 then show col ++ ". " ++ s else s
    where col = i + 1

renderTitle :: Pointer -> Int -> List -> Widget Name
renderTitle (p, _) li l =
      withAttr attr
    . addCursor li (-1) d
    $ box d

    where d = wrap width $ columnNumber li (title l)
          attr = if p == li then titleCurrentAttr else titleAttr

widget :: Pointer -> Int -> List -> Widget Name
widget p li l =
      padLeftRight padding
    . vBox
    . (renderTitle p li l :)
    . toList
    $ renderTask p li `mapWithIndex` tasks l

draw :: State -> [Widget Name]
draw s = [
          padTop (Pad 1)
        . hBox
        . toList
        $ widget (current s)  `mapWithIndex` lists s
    ]


-- app
chooseCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor s = case mode s of
    Insert (CreateList _) -> showCursorNamed (ListLocation (fst (current s), -1))
    Insert EditList -> showCursorNamed (ListLocation (fst (current s), -1))
    Insert CreateTask -> showCursorNamed (ListLocation (current s))
    Insert EditTask -> showCursorNamed (ListLocation (current s))
    _ -> neverShowCursor s

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s' (VtyEvent e) = let s = event e s' in
    case mode s of
        Shutdown -> halt s
        Write _ -> suspendAndResume $ do
            Persistence.Taskell.writeFile (lists s) (path s)
            return (Flow.State.continue s)
        _ -> Brick.continue s
handleEvent s _ = Brick.continue s

startEvent :: State -> EventM Name State
startEvent = return

attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr [
        (titleAttr, fg green),
        (titleCurrentAttr, fg blue),
        (taskCurrentAttr, fg magenta)
    ]

app :: App State e Name
app = App draw chooseCursor handleEvent startEvent attrMap'

go :: State -> IO ()
go initial = void (defaultMain app initial)
