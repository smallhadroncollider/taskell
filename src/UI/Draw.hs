module UI.Draw (
    draw,
    chooseCursor,
    scroll,
    colWidth
) where

import Flow.State (State, Mode(..), InsertMode(..), Pointer, lists, current, mode, size, normalise)
import Brick
import Data.Text (Text, length, pack, concat)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description)
import Data.Taskell.Text (wrap)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (mapWithIndex, length)

import Config

import UI.Types (ResourceName(..))
import UI.Attr

colWidth :: Int
colWidth = width + padding * 2

addCursor :: Int -> Int -> [Text] -> Widget ResourceName -> Widget ResourceName
addCursor li ti d =
      reportExtent name
    . showCursor name (Location (h, v))

    where v = Prelude.length d - 1
          h = Data.Text.length $ last d
          name = RNTask (li, ti)

box :: [Text] -> Widget ResourceName
box d = padBottom (Pad 1) . vBox $ txt <$> d

renderTask :: Pointer -> Int -> Int -> Task -> Widget ResourceName
renderTask p li ti t =
      withAttr attr
    . addCursor li ti d
    $ box d

    where d = wrap width $ description t
          attr = if (li, ti) == p then taskCurrentAttr else taskAttr

columnNumber :: Int -> Text -> Text
columnNumber i s = if col >= 1 && col <= 9 then Data.Text.concat [pack (show col), ". ",  s] else s
    where col = i + 1

renderTitle :: Pointer -> Int -> List -> Widget ResourceName
renderTitle (p, _) li l =
      withAttr attr
    . addCursor li (-1) d
    $ box d

    where d = wrap width $ columnNumber li (title l)
          attr = if p == li then titleCurrentAttr else titleAttr

renderList :: Int -> Pointer -> Int -> List -> Widget ResourceName
renderList h p li l =
      padLeftRight padding
    . hLimit width
    . viewport (RNList li) Vertical
    . padBottom (Pad h)
    . vBox
    . (renderTitle p li l :)
    . toList
    $ renderTask p li `Seq.mapWithIndex` tasks l

-- draw
draw :: State -> [Widget ResourceName]
draw state = [
          viewport RNLists Horizontal
        . padRight (Pad $ fst (size state))
        . hLimit (Seq.length ls * colWidth)
        . padTop (Pad 1)
        . hBox
        . toList
        $ renderList (snd (size state)) (current s)  `Seq.mapWithIndex` ls
    ]
    where s = normalise state
          ls = lists s

-- cursors
cursor :: (Int, Int) -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
cursor c = showCursorNamed (RNTask c)

chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode s of
    Insert (CreateList _) -> cursor (fst c, -1)
    Insert EditList -> cursor (fst c, -1)
    Insert CreateTask -> cursor c
    Insert EditTask -> cursor c
    _ -> neverShowCursor s

    where s = normalise state
          c = current s

-- scroll
scroll :: State -> EventM ResourceName ()
scroll s = do
    let (col, row) = current $ normalise s
        (w, h) = size s
    offset <- fmap sum . sequence $ fmap getHeight . lookupExtent . (\i -> RNTask (col, i)) <$> [0..row]
    setLeft (viewportScroll RNLists) $ (col * colWidth) - (w `div` 2 - colWidth `div` 2)
    setTop (viewportScroll (RNList col)) $ offset - h `div` 2

getHeight :: Maybe (Extent ResourceName) -> Int
getHeight extent = case extent of
    Nothing -> 0
    Just (Extent _ _ (_, height) _) -> height
