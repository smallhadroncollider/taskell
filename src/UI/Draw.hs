{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw (
    draw,
    chooseCursor
) where

import ClassyPrelude

import Data.Sequence (mapWithIndex)
import Control.Monad.Reader (runReader)

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

type ReaderDrawState = ReaderT DrawState Identity

renderDate :: Maybe Day -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate day = do
    today <- dsToday <$> ask
    let attr = withAttr . dlToAttr . deadline today <$> day
        widget = txt . dayToText today <$> day
    return $ attr <*> widget

renderSubTaskCount :: Task -> Widget ResourceName
renderSubTaskCount task = str $ concat ["[" , show $ countCompleteSubTasks task , "/" , show $ countSubTasks task , "]"]

indicators :: Task -> ReaderDrawState (Widget ResourceName)
indicators task = do
    dateWidget <- renderDate (due task)
    return . hBox $ padRight (Pad 1) <$> catMaybes [
            const (txt "≡") <$> summary task
          , bool Nothing (Just (renderSubTaskCount task)) (hasSubTasks task)
          , dateWidget
        ]

renderTask :: Int -> Int -> Task -> ReaderDrawState (Widget ResourceName)
renderTask listIndex taskIndex task = do
    eTitle <- dsEditingTitle <$> ask
    cur <- (== (listIndex, taskIndex)) . dsCurrent <$>ask
    taskField <- dsField <$> ask
    after <- indicators task

    let text = description task
        name = RNTask (listIndex, taskIndex)
        widget = textField text
        widget' = widgetFromMaybe widget taskField

    return $ cached name
        . (if not eTitle && cur then visible else id)
        . padBottom (Pad 1)
        . (<=> withAttr disabledAttr after)
        . withAttr (if cur then taskCurrentAttr else taskAttr)
        $ if cur && not eTitle then widget' else widget


columnNumber :: Int -> Text
columnNumber i = if col >= 1 && col <= 9 then pack (show col) ++ ". " else ""
    where col = i + 1

renderTitle :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderTitle listIndex list = do
    (p, i) <- dsCurrent <$> ask
    cur <- (p == listIndex &&) . dsEditingTitle <$> ask
    titleField <- dsField <$> ask

    let text = title list
        col = txt $ columnNumber listIndex
        attr = if p == listIndex then titleCurrentAttr else titleAttr
        widget = textField text
        widget' = widgetFromMaybe widget titleField
        title' = padBottom (Pad 1) . withAttr attr . (col <+>) $ if cur then widget' else widget

    return $ if cur || p /= listIndex || i == 0 then visible title' else title'

renderList :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderList listIndex list = do
    layout <- dsLayout <$> ask
    eTitle <- dsEditingTitle <$> ask
    titleWidget <- renderTitle listIndex list
    (currentList, _) <- dsCurrent <$> ask
    taskWidgets <- sequence $ renderTask listIndex `mapWithIndex` tasks list

    let widget = (if not eTitle then cached (RNList listIndex) else id)
            . padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList listIndex) Vertical
            . vBox
            . (titleWidget :)
            $ toList taskWidgets

    return $ if currentList == listIndex then visible widget else widget

searchImage :: Widget ResourceName -> ReaderDrawState (Widget ResourceName)
searchImage mainWidget = do
    m <- dsMode <$> ask
    case m of
        Search editing searchField -> do
            colPad <- columnPadding . dsLayout <$> ask
            let attr = withAttr $ if editing then taskCurrentAttr else taskAttr
            let widget = attr . padTopBottom 1 . padLeftRight colPad $ txt "/" <+> field searchField
            return $ mainWidget <=> widget
        _ -> return mainWidget

main :: ReaderDrawState (Widget ResourceName)
main = do
    ls <- dsLists <$> ask
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    let mainWidget = viewport RNLists Horizontal . padTopBottom 1 $ hBox listWidgets
    searchImage mainWidget

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _ = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _ = False

-- draw
draw :: LayoutConfig -> Day -> State -> [Widget ResourceName]
draw layout today state =
    showModal normalisedState today [runReader main DrawState {
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
