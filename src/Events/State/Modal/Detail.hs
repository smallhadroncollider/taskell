{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.State.Modal.Detail
    ( updateField
    , finishSubtask
    , finishDescription
    , finishDue
    , showDetail
    , getCurrentItem
    , getCurrentMode
    , getField
    , setComplete
    , remove
    , insertMode
    , editDescription
    , editDue
    , newItem
    , nextSubtask
    , previousSubtask
    , lastSubtask
    ) where

import ClassyPrelude

import Control.Lens ((&), (.~), (^.))

import           Data.Taskell.Date       (dayToOutput)
import qualified Data.Taskell.Subtask    as ST (blank, name, toggle)
import           Data.Taskell.Task       (Task, addSubtask, countSubtasks, description, due,
                                          getSubtask, removeSubtask, setDescription, setDue,
                                          updateSubtask)
import           Events.State            (getCurrentTask, setCurrentTask)
import           Events.State.Types      (State, Stateful, mode)
import           Events.State.Types.Mode (DetailItem (..), DetailMode (..), ModalType (Detail),
                                          Mode (Modal))
import           UI.Field                (Field, blankField, getText, textToField)

updateField :: (Field -> Field) -> Stateful
updateField fieldEvent s =
    pure $
    case s ^. mode of
        Modal (Detail detailItem (DetailInsert field)) ->
            s & mode .~ Modal (Detail detailItem (DetailInsert (fieldEvent field)))
        _ -> s

finishSubtask :: Stateful
finishSubtask state = do
    text <- getText <$> getField state
    i <- getCurrentSubtask state
    task <- updateSubtask i (ST.name .~ text) <$> getCurrentTask state
    setCurrentTask task $ state & mode .~ Modal (Detail (DetailItem i) (DetailInsert blankField))

finish :: (Text -> Task -> Task) -> Stateful
finish fn state = do
    text <- getText <$> getField state
    task <- fn text <$> getCurrentTask state
    setCurrentTask task $ state & mode .~ Modal (Detail (DetailItem 0) DetailNormal)

finishDescription :: Stateful
finishDescription = finish setDescription

finishDue :: Stateful
finishDue = finish setDue

showDetail :: Stateful
showDetail s = do
    _ <- getCurrentTask s
    let i = fromMaybe 0 $ getCurrentSubtask s
    pure $ s & mode .~ Modal (Detail (DetailItem i) DetailNormal)

getCurrentSubtask :: State -> Maybe Int
getCurrentSubtask state =
    case state ^. mode of
        Modal (Detail (DetailItem i) _) -> Just i
        _                               -> Nothing

getCurrentItem :: State -> Maybe DetailItem
getCurrentItem state =
    case state ^. mode of
        Modal (Detail item _) -> Just item
        _                     -> Nothing

getCurrentMode :: State -> Maybe DetailMode
getCurrentMode state =
    case state ^. mode of
        Modal (Detail _ m) -> Just m
        _                  -> Nothing

getField :: State -> Maybe Field
getField state =
    case state ^. mode of
        Modal (Detail _ (DetailInsert f)) -> Just f
        _                                 -> Nothing

setComplete :: Stateful
setComplete state = do
    i <- getCurrentSubtask state
    task <- updateSubtask i ST.toggle <$> getCurrentTask state
    setCurrentTask task state

remove :: Stateful
remove state = do
    i <- getCurrentSubtask state
    task <- removeSubtask i <$> getCurrentTask state
    state' <- setCurrentTask task state
    setIndex state' i

insertMode :: Stateful
insertMode state = do
    i <- getCurrentSubtask state
    task <- getCurrentTask state
    n <- (^. ST.name) <$> getSubtask i task
    case state ^. mode of
        Modal (Detail (DetailItem i') _) ->
            Just (state & mode .~ Modal (Detail (DetailItem i') (DetailInsert (textToField n))))
        _ -> Nothing

editDescription :: Stateful
editDescription state = do
    summ <- (^. description) <$> getCurrentTask state
    let summ' = fromMaybe "" summ
    pure $ state & mode .~ Modal (Detail DetailDescription (DetailInsert (textToField summ')))

editDue :: Stateful
editDue state = do
    day <- (^. due) <$> getCurrentTask state
    let day' = maybe "" dayToOutput day
    pure $ state & mode .~ Modal (Detail DetailDate (DetailInsert (textToField day')))

newItem :: Stateful
newItem state = do
    task <- addSubtask ST.blank <$> getCurrentTask state
    setCurrentTask task state

-- list navigation
changeSubtask :: Int -> Stateful
changeSubtask inc state = do
    i <- (+ inc) <$> getCurrentSubtask state
    setIndex state i

nextSubtask :: Stateful
nextSubtask = changeSubtask 1

previousSubtask :: Stateful
previousSubtask = changeSubtask (-1)

lastSubtask :: Stateful
lastSubtask state = lastIndex state >>= setIndex state

lastIndex :: State -> Maybe Int
lastIndex state = (+ (-1)) . countSubtasks <$> getCurrentTask state

setIndex :: State -> Int -> Maybe State
setIndex state i = do
    lst <- lastIndex state
    m <- getCurrentMode state
    let newIndex
            | i > lst = lst
            | i < 0 = 0
            | otherwise = i
    return $ state & mode .~ Modal (Detail (DetailItem newIndex) m)
