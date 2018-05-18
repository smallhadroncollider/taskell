{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw (
    draw,
    chooseCursor
) where

import ClassyPrelude

import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.Date (Day, DeadlineFn, Deadline(..), dayToText)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description, hasSubTasks, countSubTasks, countCompleteSubTasks, summary, due)
import Events.State (lists, current, mode, normalise)
import Events.State.Types (State, Mode(..), InsertType(..), Pointer, ModalType(..), SubTasksMode(..))
import IO.Config (LayoutConfig, columnWidth, columnPadding)
import UI.Field (Field, field, textField, widgetFromMaybe)
import UI.Modal (showModal)
import UI.Theme
import UI.Types (ResourceName(..))

dlToAttr :: Deadline -> AttrName
dlToAttr dl = case dl of
    Plenty -> dlFar
    ThisWeek -> dlSoon
    Tomorrow -> dlSoon
    Today -> dlDue
    Passed -> dlDue

renderDate :: DeadlineFn -> Maybe Day -> Maybe (Widget ResourceName)
renderDate deadlineFn day = do
    attr <- withAttr . dlToAttr <$> deadlineFn day
    widget <- txt . dayToText <$> day
    return $ attr widget

renderSubTaskCount :: Task -> Widget ResourceName
renderSubTaskCount t = str $ concat [
        "["
      , show $ countCompleteSubTasks t
      , "/"
      , show $ countSubTasks t
      , "]"
    ]

indicators :: DeadlineFn -> Task -> Widget ResourceName
indicators deadlineFn t = hBox $ padRight (Pad 1) <$> widgets
    where summ = const (txt "≡") <$> summary t
          sts = bool Nothing (Just (renderSubTaskCount t)) (hasSubTasks t)
          dl = renderDate deadlineFn (due t)
          widgets = catMaybes [summ, sts, dl]

renderTask :: DeadlineFn -> Maybe Field -> Bool -> Pointer -> Int -> Int -> Task -> Widget ResourceName
renderTask deadlineFn f eTitle p li ti t =
      cached name
    . (if not eTitle && cur then visible else id)
    . padBottom (Pad 1)
    . (<=> withAttr disabledAttr after)
    . withAttr (if cur then taskCurrentAttr else taskAttr)
    $ if cur && not eTitle then widget' else widget

    where cur = (li, ti) == p
          text = description t
          after = indicators deadlineFn t
          name = RNTask (li, ti)
          widget = textField text
          widget' = widgetFromMaybe widget f

columnNumber :: Int -> Text
columnNumber i = if col >= 1 && col <= 9 then pack (show col) ++ ". " else ""
    where col = i + 1

renderTitle :: Maybe Field -> Bool -> Pointer -> Int -> List -> Widget ResourceName
renderTitle f eTitle (p, i) li l =
    if cur || p /= li || i == 0
        then visible title'
        else title'

    where cur = p == li && eTitle
          text = title l
          col = txt $ columnNumber li
          attr = if p == li then titleCurrentAttr else titleAttr
          title' = padBottom (Pad 1) . withAttr attr . (col <+>) $ if cur then widget' else widget
          widget = textField text
          widget' = widgetFromMaybe widget f

renderList :: LayoutConfig -> DeadlineFn -> Maybe Field -> Bool -> Pointer -> Int -> List -> Widget ResourceName
renderList layout deadlineFn f eTitle p li l = if fst p == li then visible list else list
    where list =
              (if not eTitle then cached (RNList li) else id)
            . padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList li) Vertical
            . vBox
            . (renderTitle f eTitle p li l :)
            . toList
            $ renderTask deadlineFn f eTitle p li `mapWithIndex` tasks l

searchImage :: LayoutConfig -> State -> Widget ResourceName -> Widget ResourceName
searchImage layout s i = case mode s of
    Search ent f ->
        let attr = if ent then taskCurrentAttr else taskAttr
        in
            i <=> (
                  withAttr attr
                . padTopBottom 1
                . padLeftRight (columnPadding layout)
                $ txt "/" <+> field f
            )
    _ -> i

main :: LayoutConfig -> DeadlineFn -> State -> Widget ResourceName
main layout deadlineFn s =
      searchImage layout s
    . viewport RNLists Horizontal
    . padTopBottom 1
    . hBox
    . toList
    $ renderList layout deadlineFn (getField s) (editingTitle s) (current s)  `mapWithIndex` ls

    where ls = lists s

getField :: State -> Maybe Field
getField state = case mode state of
    Insert _ _ f -> Just f
    _ -> Nothing


editingTitle :: State -> Bool
editingTitle state = case mode state of
    Insert IList _ _ -> True
    _ -> False

-- draw
draw :: LayoutConfig -> DeadlineFn -> State -> [Widget ResourceName]
draw layout deadlineFn state =
    let s = normalise state in
    showModal s [main layout deadlineFn s]

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode (normalise state) of
    Insert {} -> showCursorNamed RNCursor
    Search True _ -> showCursorNamed RNCursor
    Modal (SubTasks _ (STInsert _)) -> showCursorNamed RNCursor
    _ -> neverShowCursor state
