{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw (
    draw,
    chooseCursor
) where

import ClassyPrelude

import Data.Sequence (mapWithIndex)

import Brick

import Data.Taskell.Date (Day, dayToText, deadline)
import Data.Taskell.Lists (Lists)
import Data.Taskell.List (List, tasks, title)
import Data.Taskell.Task (Task, description, hasSubTasks, countSubTasks, countCompleteSubTasks, summary, due)
import Events.State (lists, current, mode, normalise)
import Events.State.Types (State, Mode(..), InsertType(..), Pointer, ModalType(..), DetailMode(..))
import IO.Config (LayoutConfig, columnWidth, columnPadding)
import UI.Field (Field, field, textField, widgetFromMaybe)
import UI.Modal (showModal)
import UI.Theme
import UI.Types (ResourceName(..))

data DrawState = DrawState {
    dsLists :: Lists
  , dsMode :: Mode
  , dsLayout :: LayoutConfig
  , dsToday :: Day
  , dsCurrent :: Pointer
  , dsField :: Maybe Field
  , dsEditingTitle :: Bool
}

renderDate :: Day -> Maybe Day -> Maybe (Widget ResourceName)
renderDate today day = do
    attr <- withAttr . dlToAttr . deadline today <$> day
    widget <- txt . dayToText today <$> day
    return $ attr widget

renderSubTaskCount :: Task -> Widget ResourceName
renderSubTaskCount task = str $ concat [
        "["
      , show $ countCompleteSubTasks task
      , "/"
      , show $ countSubTasks task
      , "]"
    ]

indicators :: Day -> Task -> Widget ResourceName
indicators today task = hBox $ padRight (Pad 1) <$> catMaybes [
        const (txt "≡") <$> summary task
      , bool Nothing (Just (renderSubTaskCount task)) (hasSubTasks task)
      , renderDate today (due task)
    ]

renderTask :: DrawState -> Int -> Int -> Task -> Widget ResourceName
renderTask drawState listIndex taskIndex task =
      cached name
    . (if not eTitle && cur then visible else id)
    . padBottom (Pad 1)
    . (<=> withAttr disabledAttr after)
    . withAttr (if cur then taskCurrentAttr else taskAttr)
    $ if cur && not eTitle then widget' else widget

    where eTitle = dsEditingTitle drawState
          cur = (listIndex, taskIndex) == dsCurrent drawState
          text = description task
          after = indicators (dsToday drawState) task
          name = RNTask (listIndex, taskIndex)
          widget = textField text
          widget' = widgetFromMaybe widget $ dsField drawState

columnNumber :: Int -> Text
columnNumber i = if col >= 1 && col <= 9 then pack (show col) ++ ". " else ""
    where col = i + 1

renderTitle :: DrawState -> Int -> List -> Widget ResourceName
renderTitle drawState listIndex list =
    if cur || p /= listIndex || i == 0 then visible title' else title'

    where (p, i) = dsCurrent drawState
          cur = p == listIndex && dsEditingTitle drawState
          text = title list
          col = txt $ columnNumber listIndex
          attr = if p == listIndex then titleCurrentAttr else titleAttr
          title' = padBottom (Pad 1) . withAttr attr . (col <+>) $ if cur then widget' else widget
          widget = textField text
          widget' = widgetFromMaybe widget $ dsField drawState

renderList :: DrawState -> Int -> List -> Widget ResourceName
renderList drawState listIndex list =
    if fst (dsCurrent drawState) == listIndex then visible widget else widget

    where layout = dsLayout drawState
          widget =
              (if not (dsEditingTitle drawState) then cached (RNList listIndex) else id)
            . padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList listIndex) Vertical
            . vBox
            . (renderTitle drawState listIndex list :)
            . toList
            $ renderTask drawState listIndex `mapWithIndex` tasks list

searchImage :: DrawState -> Widget ResourceName -> Widget ResourceName
searchImage drawState mainWidget = case dsMode drawState of
    Search editing searchField ->
        let attr = if editing then taskCurrentAttr else taskAttr
        in
            mainWidget <=> (
                  withAttr attr
                . padTopBottom 1
                . padLeftRight (columnPadding (dsLayout drawState))
                $ txt "/" <+> field searchField
            )
    _ -> mainWidget

main :: DrawState -> Widget ResourceName
main drawState =
      searchImage drawState
    . viewport RNLists Horizontal
    . padTopBottom 1
    . hBox
    . toList
    $ renderList drawState `mapWithIndex` dsLists drawState

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _ = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _ = False

-- draw
draw :: LayoutConfig -> Day -> State -> [Widget ResourceName]
draw layout today state =
    showModal normalisedState today [main DrawState {
        dsLists = lists normalisedState
      , dsMode = stateMode
      , dsLayout = layout
      , dsToday = today
      , dsField = getField stateMode
      , dsCurrent = current normalisedState
      , dsEditingTitle = editingTitle stateMode
    }]

    where normalisedState = normalise state
          stateMode = mode state

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case mode (normalise state) of
    Insert {} -> showCursorNamed RNCursor
    Search True _ -> showCursorNamed RNCursor
    Modal (Detail _ (DetailInsert _)) -> showCursorNamed RNCursor
    _ -> neverShowCursor state
