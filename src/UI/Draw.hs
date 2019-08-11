{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UI.Draw
    ( draw
    , chooseCursor
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Control.Monad.Reader (runReader)
import Data.Char            (chr, ord)
import Data.Sequence        (mapWithIndex)

import Brick

import           Data.Taskell.Date       (Day, dayToText, deadline)
import           Data.Taskell.List       (List, tasks, title)
import           Data.Taskell.Lists      (count)
import qualified Data.Taskell.Task       as T (Task, contains, countCompleteSubtasks, countSubtasks,
                                               description, due, hasSubtasks, name)
import           Events.State            (normalise)
import           Events.State.Types      (State, current, height, lists, mode, path, searchTerm)
import           Events.State.Types.Mode (DetailMode (..), InsertType (..), ModalType (..),
                                          Mode (..))
import           IO.Config.Layout        (Config, columnPadding, columnWidth, descriptionIndicator)
import           IO.Keyboard.Types       (Bindings)
import           UI.Draw.Types           (DrawState (..), ReaderDrawState)
import           UI.Field                (Field, field, getText, textField, widgetFromMaybe)
import           UI.Modal                (showModal)
import           UI.Theme
import           UI.Types                (ListIndex (..), ResourceName (..), TaskIndex (..))

-- | Takes a task's 'due' property and renders a date with appropriate styling (e.g. red if overdue)
renderDate :: Maybe Day -> ReaderDrawState (Maybe (Widget ResourceName))
renderDate dueDay = do
    today <- dsToday <$> ask -- get the value of `today` from DrawState
    let attr = withAttr . dlToAttr . deadline today <$> dueDay -- create a `Maybe (Widget -> Widget)` attribute function
        widget = txt . dayToText today <$> dueDay -- get the formatted due date `Maybe Text`
    pure $ attr <*> widget

-- | Renders the appropriate completed sub task count e.g. "[2/3]"
renderSubtaskCount :: T.Task -> Widget ResourceName
renderSubtaskCount task =
    txt $ concat ["[", tshow $ T.countCompleteSubtasks task, "/", tshow $ T.countSubtasks task, "]"]

-- | Renders the appropriate indicators: description, sub task count, and due date
indicators :: T.Task -> ReaderDrawState (Widget ResourceName)
indicators task = do
    dateWidget <- renderDate (task ^. T.due) -- get the due date widget
    descIndicator <- descriptionIndicator . dsLayout <$> ask
    pure . hBox $
        padRight (Pad 1) <$>
        catMaybes
            [ const (txt descIndicator) <$> task ^. T.description -- show the description indicator if one is set
            , bool Nothing (Just (renderSubtaskCount task)) (T.hasSubtasks task) -- if it has subtasks, render the sub task count
            , dateWidget
            ]

-- | Renders an individual task
renderTask' :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask' listIndex taskIndex task = do
    eTitle <- dsEditingTitle <$> ask -- is the title being edited? (for visibility)
    selected <- (== (listIndex, taskIndex)) . dsCurrent <$> ask -- is the current task selected?
    taskField <- dsField <$> ask -- get the field, if it's being edited
    after <- indicators task -- get the indicators widget
    let text = task ^. T.name
        name = RNTask (ListIndex listIndex, TaskIndex taskIndex)
        widget = textField text
        widget' = widgetFromMaybe widget taskField
    pure $
        cached name .
        (if selected && not eTitle
             then visible
             else id) .
        padBottom (Pad 1) .
        (<=> withAttr disabledAttr after) .
        withAttr
            (if selected
                 then taskCurrentAttr
                 else taskAttr) $
        if selected && not eTitle
            then widget'
            else widget

renderTask :: Int -> Int -> T.Task -> ReaderDrawState (Widget ResourceName)
renderTask listIndex taskIndex task = do
    searchT <- asks dsSearchTerm
    case searchT of
        Nothing -> renderTask' listIndex taskIndex task
        Just term ->
            if T.contains (getText term) task
                then renderTask' listIndex taskIndex task
                else pure emptyWidget

-- | Gets the relevant column prefix - number in normal mode, letter in moveTo
columnPrefix :: Int -> Int -> ReaderDrawState Text
columnPrefix selectedList i = do
    m <- dsMode <$> ask
    if moveTo m
        then do
            let col = chr (i + ord 'a')
            pure $
                if i /= selectedList && i >= 0 && i <= 26
                    then singleton col <> ". "
                    else ""
        else do
            let col = i + 1
            pure $
                if col >= 1 && col <= 9
                    then tshow col <> ". "
                    else ""

-- | Renders the title for a list
renderTitle :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderTitle listIndex list = do
    (selectedList, selectedTask) <- dsCurrent <$> ask
    editing <- (selectedList == listIndex &&) . dsEditingTitle <$> ask
    titleField <- dsField <$> ask
    col <- txt <$> columnPrefix selectedList listIndex
    let text = list ^. title
        attr =
            if selectedList == listIndex
                then titleCurrentAttr
                else titleAttr
        widget = textField text
        widget' = widgetFromMaybe widget titleField
        title' =
            padBottom (Pad 1) . withAttr attr . (col <+>) $
            if editing
                then widget'
                else widget
    pure $
        if editing || selectedList /= listIndex || selectedTask == 0
            then visible title'
            else title'

-- | Renders a list
renderList :: Int -> List -> ReaderDrawState (Widget ResourceName)
renderList listIndex list = do
    layout <- dsLayout <$> ask
    eTitle <- dsEditingTitle <$> ask
    titleWidget <- renderTitle listIndex list
    (currentList, _) <- dsCurrent <$> ask
    taskWidgets <- sequence $ renderTask listIndex `mapWithIndex` (list ^. tasks)
    let widget =
            (if not eTitle
                 then cached (RNList listIndex)
                 else id) .
            padLeftRight (columnPadding layout) .
            hLimit (columnWidth layout) .
            viewport (RNList listIndex) Vertical . vBox . (titleWidget :) $
            toList taskWidgets
    pure $
        if currentList == listIndex
            then visible widget
            else widget

-- | Renders the search area
renderSearch :: Widget ResourceName -> ReaderDrawState (Widget ResourceName)
renderSearch mainWidget = do
    m <- asks dsMode
    term <- asks dsSearchTerm
    case term of
        Just searchField -> do
            colPad <- columnPadding . dsLayout <$> ask
            let attr =
                    withAttr $
                    case m of
                        Search -> taskCurrentAttr
                        _      -> taskAttr
            let widget = attr . padLeftRight colPad $ txt "/" <+> field searchField
            pure $ mainWidget <=> widget
        _ -> pure mainWidget

-- | Render the status bar
getPosition :: ReaderDrawState Text
getPosition = do
    (col, pos) <- asks dsCurrent
    len <- count col <$> asks dsLists
    let posNorm =
            if len > 0
                then pos + 1
                else 0
    pure $ tshow posNorm <> "/" <> tshow len

modeToText :: Maybe Field -> Mode -> Text
modeToText fld =
    \case
        Normal ->
            case fld of
                Nothing -> "NORMAL"
                Just _  -> "NORMAL + SEARCH"
        Insert {} -> "INSERT"
        Modal Help -> "HELP"
        Modal MoveTo -> "MOVE"
        Modal Detail {} -> "DETAIL"
        Search {} -> "SEARCH"
        _ -> ""

getMode :: ReaderDrawState Text
getMode = modeToText <$> asks dsSearchTerm <*> asks dsMode

renderStatusBar :: ReaderDrawState (Widget ResourceName)
renderStatusBar = do
    topPath <- pack <$> asks dsPath
    colPad <- columnPadding <$> asks dsLayout
    posTxt <- getPosition
    modeTxt <- getMode
    let titl = padLeftRight colPad $ txt topPath
    let pos = padRight (Pad colPad) $ txt posTxt
    let md = txt modeTxt
    let bar = padRight Max (titl <+> md) <+> pos
    pure . padTop (Pad 1) $ withAttr statusBarAttr bar

-- | Renders the main widget
main :: ReaderDrawState (Widget ResourceName)
main = do
    ls <- dsLists <$> ask
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    let mainWidget = viewport RNLists Horizontal . padTopBottom 1 $ hBox listWidgets
    statusBar <- renderStatusBar
    renderSearch (mainWidget <=> statusBar)

getField :: Mode -> Maybe Field
getField (Insert _ _ f) = Just f
getField _              = Nothing

editingTitle :: Mode -> Bool
editingTitle (Insert IList _ _) = True
editingTitle _                  = False

moveTo :: Mode -> Bool
moveTo (Modal MoveTo) = True
moveTo _              = False

-- draw
drawR :: ReaderDrawState [Widget ResourceName]
drawR = do
    modal <- showModal
    mn <- main
    pure [modal, mn]

draw :: Config -> Bindings -> Day -> State -> [Widget ResourceName]
draw layout bindings today state = runReader drawR drawState
  where
    normalisedState = normalise state
    stateMode = state ^. mode
    drawState =
        DrawState
        { dsLists = normalisedState ^. lists
        , dsMode = stateMode
        , dsLayout = layout
        , dsPath = normalisedState ^. path
        , dsToday = today
        , dsField = getField stateMode
        , dsCurrent = normalisedState ^. current
        , dsEditingTitle = editingTitle stateMode
        , dsSearchTerm = normalisedState ^. searchTerm
        , dsHeight = state ^. height
        , dsBindings = bindings
        }

-- cursors
chooseCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
chooseCursor state =
    case normalise state ^. mode of
        Insert {}                         -> showCursorNamed RNCursor
        Search                            -> showCursorNamed RNCursor
        Modal (Detail _ (DetailInsert _)) -> showCursorNamed RNCursor
        _                                 -> neverShowCursor state
