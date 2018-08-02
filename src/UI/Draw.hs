{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Draw (
    draw,
    chooseCursor
) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Char (ord, chr)
import Data.Sequence (mapWithIndex)
import Control.Monad.Reader (runReader)

import Brick

import Data.Taskell.Date (Day, dayToText, deadline)
import Data.Taskell.Lists (Lists)
import Data.Taskell.List (List, tasks, title)
import qualified Data.Taskell.Task as T (Task, name, hasSubtasks, countSubtasks, countCompleteSubtasks, description, due)
import Events.State (normalise)
import Events.State.Types (State, Pointer, lists, current, mode)
import Events.State.Types.Mode (Mode(..), InsertType(..), ModalType(..), DetailMode(..))
import IO.Config.Layout (Config, columnWidth, columnPadding, descriptionIndicator)
import UI.Field (Field, field, textField, widgetFromMaybe)
import UI.Modal (showModal)
import UI.Theme
import UI.Types (ResourceName(..))

-- | Draw needs to know various pieces of information, so keep track of them in a record
data DrawState = DrawState {
    dsLists :: Lists
  , dsMode :: Mode
  , dsLayout :: Config
  , dsToday :: Day
  , dsCurrent :: Pointer
  , dsField :: Maybe Field
  , dsEditingTitle :: Bool
}

-- | Use a Reader to pass around DrawState
type ReaderDrawState = ReaderT DrawState Identity

-- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
renderDate :: Maybe Day -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate dueDay = do
    today <- dsToday <$> ask -- get the value of `today` from DrawState
    let attr = withAttr . dlToAttr . deadline today <$> dueDay -- create a `Maybe (Widget -> Widget)` attribute function
        widget = txt . dayToText today <$> dueDay -- get the formatted due date `Maybe Text`
    return $ attr <*> widget

-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> Widget ResourceName
renderSubtaskCount task = txt $ concat ["[" , tshow $ T.countCompleteSubtasks task , "/" , tshow $ T.countSubtasks task , "]"]

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> ReaderDrawState (Widget ResourceName)
indicators task = do
    dateWidget <- renderDate (task ^. T.due) -- get the due date widget
    descIndicator <- descriptionIndicator . dsLayout <$> ask
    return . hBox $ padRight (Pad 1) <$> catMaybes [
            const (txt descIndicator) <$> task ^. T.description -- show the description indicator if one is set
          , bool Nothing (Just (renderSubtaskCount task)) (T.hasSubtasks task) -- if it has subtasks, render the sub task count
          , dateWidget
        ]

-- | Renders an individual task
renderTask :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask listIndex taskIndex task = do
    eTitle <- dsEditingTitle <$> ask -- is the title being edited? (for visibility)
    selected <- (== (listIndex, taskIndex)) . dsCurrent <$>ask -- is the current task selected?
    taskField <- dsField <$> ask -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget

    let text = task ^. T.name
        name = RNTask (listIndex, taskIndex)
        widget = textField text
        widget' = widgetFromMaybe widget taskField

    return $ cached name
        . (if selected && not eTitle then visible else id)
        . padBottom (Pad 1)
        . (<=> withAttr disabledAttr after)
        . withAttr (if selected then taskCurrentAttr else taskAttr)
        $ if selected && not eTitle then widget' else widget


-- | Gets the relevant column prefix - number in normal mode, letter in moveTo
columnPrefix :: Int -> Int -> ReaderDrawState Text
columnPrefix selectedList i = do
    m <- dsMode <$> ask
    if moveTo m
        then do
            let col = chr (i + ord 'a')
            return $ if i /= selectedList && i >= 0 && i <= 26 then singleton col ++ ". " else ""
        else do
            let col = i + 1
            return $ if col >= 1 && col <= 9 then tshow col ++ ". " else ""

-- | Renders the title for a list
renderTitle :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderTitle listIndex list = do
    (selectedList, selectedTask) <- dsCurrent <$> ask
    editing <- (selectedList == listIndex &&) . dsEditingTitle <$> ask
    titleField <- dsField <$> ask
    col <- txt <$> columnPrefix selectedList listIndex

    let text = list ^. title
        attr = if selectedList == listIndex then titleCurrentAttr else titleAttr
        widget = textField text
        widget' = widgetFromMaybe widget titleField
        title' = padBottom (Pad 1) . withAttr attr . (col <+>) $ if editing then widget' else widget

    return $ if editing || selectedList /= listIndex || selectedTask == 0 then visible title' else title'

-- | Renders a list
renderList :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderList listIndex list = do
    layout <- dsLayout <$> ask
    eTitle <- dsEditingTitle <$> ask
    titleWidget <- renderTitle listIndex list
    (currentList, _) <- dsCurrent <$> ask
    taskWidgets <- sequence $ renderTask listIndex `mapWithIndex` (list ^. tasks)

    let widget = (if not eTitle then cached (RNList listIndex) else id)
            . padLeftRight (columnPadding layout)
            . hLimit (columnWidth layout)
            . viewport (RNList listIndex) Vertical
            . vBox
            . (titleWidget :)
            $ toList taskWidgets

    return $ if currentList == listIndex then visible widget else widget

-- | Renders the search area
renderSearch :: Widget ResourceName -> ReaderDrawState (Widget ResourceName)
renderSearch mainWidget = do
    m <- dsMode <$> ask
    case m of
        Search editing searchField -> do
            colPad <- columnPadding . dsLayout <$> ask
            let attr = withAttr $ if editing then taskCurrentAttr else taskAttr
            let widget = attr . padTopBottom 1 . padLeftRight colPad $ txt "/" <+> field searchField
            return $ mainWidget <=> widget
        _ -> return mainWidget

-- | Renders the main widget
main :: ReaderDrawState (Widget ResourceName)
main = do
    ls <- dsLists <$> ask
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    let mainWidget = viewport RNLists Horizontal . padTopBottom 1 $ hBox listWidgets
    renderSearch mainWidget

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _ = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _ = False

moveTo :: Mode -> Bool
moveTo (Modal MoveTo) = True
moveTo _ = False

-- draw
draw :: Config -> Day -> State -> [Widget ResourceName]
draw layout today state =
    showModal normalisedState today [runReader main DrawState {
        dsLists = normalisedState ^. lists
      , dsMode = stateMode
      , dsLayout = layout
      , dsToday = today
      , dsField = getField stateMode
      , dsCurrent = normalisedState ^. current
      , dsEditingTitle = editingTitle stateMode
    }]

    where normalisedState = normalise state
          stateMode = state ^. mode

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state = case normalise state ^. mode of
    Insert {} -> showCursorNamed RNCursor
    Search True _ -> showCursorNamed RNCursor
    Modal (Detail _ (DetailInsert _)) -> showCursorNamed RNCursor
    _ -> neverShowCursor state
