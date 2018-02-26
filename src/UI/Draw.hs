{-# LANGUAGE OverloadedStrings #-}
module UI.Draw (
    draw,
    chooseCursor
) where

import Events.State (lists, current, mode, normalise)
import Events.State.Types (State, Mode(..), InsertMode(..), Pointer, ModalType(..), SubTasksMode(..))
import Brick
import Data.Text as T (Text, length, pack, concat, append, null)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description, hasSubTasks, countSubTasks, countCompleteSubTasks)
import Data.Taskell.Text (wrap)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (mapWithIndex, length)

import IO.Config (LayoutConfig, columnWidth, columnPadding)

import UI.Modal (showModal)
import UI.Types (ResourceName(..))
import UI.Theme
import UI.Internal (box)

colWidth :: LayoutConfig -> Int
colWidth layout = columnWidth layout + columnPadding layout * 2

addCursor :: Int -> Int -> Int -> [Text] -> Widget ResourceName -> Widget ResourceName
addCursor width li ti d = showCursor name (Location location)

    where v = Prelude.length d - 1
          h = T.length $ last d
          location = if h >= width then (0, v + 1) else (h, v)
          name = RNTask (li, ti)


blank :: Int -> Int -> Widget ResourceName
blank li ti = showCursor (RNTask (li, ti)) (Location (0, 0)) . padBottom (Pad 1) $ txt " "

subTaskCount :: Task -> Widget ResourceName
subTaskCount t
    | hasSubTasks t = str $ Prelude.concat ["[", show $ countCompleteSubTasks t, "/", show $ countSubTasks t, "]"]
    | otherwise = emptyWidget

renderTask :: LayoutConfig -> Pointer -> Int -> Int -> Task -> Widget ResourceName
renderTask layout p li ti t = if T.null text then blank li ti else
      cached (RNTask (li, ti))
    . (if cur then visible else id)
    . padBottom (Pad 1)
    . (<=> withAttr disabledAttr after)
    . withAttr (if cur then taskCurrentAttr else taskAttr)
    . addCursor width li ti d
    . vBox $ txt <$> d

    where cur = (li, ti) == p
          text = description t
          after = subTaskCount t
          width = columnWidth layout
          d = wrap width text

columnNumber :: Int -> Text -> Text
columnNumber i s = if col >= 1 && col <= 9 then T.concat [pack (show col), ". ",  s] else s
    where col = i + 1

renderTitle :: LayoutConfig -> Pointer -> Int -> List -> Widget ResourceName
renderTitle layout (p, i) li l = if p /= li || i == 0 then visible title' else title'

    where width = columnWidth layout
          d = wrap width  $ columnNumber li (title l)
          attr = if p == li then titleCurrentAttr else titleAttr
          title' = withAttr attr . addCursor width li (-1) d $ box 1 d

renderList :: LayoutConfig -> Pointer -> Int -> List -> Widget ResourceName
renderList layout p li l = if fst p == li then visible list else list
    where list =
              cached (RNList li)
            . padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList li) Vertical
            . vBox
            . (renderTitle layout p li l :)
            . toList
            $ renderTask layout p li `Seq.mapWithIndex` tasks l

searchImage :: LayoutConfig -> State -> Widget ResourceName -> Widget ResourceName
searchImage layout s i = case mode s of
    Search ent term ->
        let attr = if ent then taskCurrentAttr else taskAttr
        in
            i <=> (
                  withAttr attr
                . padTopBottom 1
                . padLeftRight (columnPadding layout)
                $ txt ("/" `append` term)
            )
    _ -> i

main :: LayoutConfig -> State -> Widget ResourceName
main layout s =
      searchImage layout s
    . viewport RNLists Horizontal
    . hLimit (Seq.length ls * colWidth layout)
    . padTopBottom 1
    . hBox
    . toList
    $ renderList layout (current s)  `Seq.mapWithIndex` ls

    where ls = lists s

-- draw
draw :: LayoutConfig -> State -> [Widget ResourceName]
draw layout state =
    let s = normalise state in
    showModal s [main layout s]

-- cursors
cursor :: (Int, Int) -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
cursor c = showCursorNamed (RNTask c)

chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode s of
    Insert (CreateList _) -> cursor (fst c, -1)
    Insert EditList -> cursor (fst c, -1)
    Insert CreateTask -> cursor c
    Insert EditTask -> cursor c
    Modal (SubTasks i STInsert) -> showCursorNamed (RNModalItem i)
    _ -> neverShowCursor s

    where s = normalise state
          c = current s
