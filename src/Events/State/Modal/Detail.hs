{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Events.State.Modal.Detail where

import ClassyPrelude

import Data.Taskell.Date (dayToOutput)
import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (Task, updateSubTask, toggleComplete, addSubTask, blankSubTask, countSubTasks, removeSubTask, setSubTaskName, name, getSubTask, summary, setSummary, due, setDue)
import UI.Field (Field, blankField, getText, textToField)

updateField :: (Field -> Field) -> Stateful
updateField fieldEvent s = return $ case mode s of
    Modal (Detail detailItem (DetailInsert field)) -> s {
        mode = Modal (Detail detailItem (DetailInsert (fieldEvent field)))
    }
    _ -> s

editingSummary :: State -> Bool
editingSummary s = case mode s of
    Modal (Detail DetailDescription _) -> True
    _ -> False

finishSubTask :: Stateful
finishSubTask state = do
    text <- getText <$> getField state
    i <- getCurrentSubTask state
    task <- updateSubTask i (setSubTaskName text) <$> getCurrentTask state
    setCurrentTask task $ state { mode = Modal (Detail (DetailItem i) (DetailInsert blankField)) }

finish :: (Text -> Task -> Task) -> Stateful
finish fn state = do
    text <- getText <$> getField state
    task <- fn text <$> getCurrentTask state
    setCurrentTask task $ state { mode = Modal (Detail (DetailItem 0) DetailNormal) }

finishSummary :: Stateful
finishSummary = finish setSummary

finishDue :: Stateful
finishDue = finish setDue

showDetail :: Stateful
showDetail s = do
    _ <- getCurrentTask s
    let i = fromMaybe 0 $ getCurrentSubTask s
    return $ s { mode = Modal (Detail (DetailItem i) DetailNormal) }

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (Detail (DetailItem i) _) -> Just i
    _ -> Nothing

getCurrentItem :: State -> Maybe DetailItem
getCurrentItem state = case mode state of
    Modal (Detail item _) -> Just item
    _ -> Nothing

getCurrentMode :: State -> Maybe DetailMode
getCurrentMode state = case mode state of
    Modal (Detail _ m) -> Just m
    _ -> Nothing

getField :: State -> Maybe Field
getField state = case mode state of
    Modal (Detail _ (DetailInsert f)) -> Just f
    _ -> Nothing

setComplete :: Stateful
setComplete state = do
    i <- getCurrentSubTask state
    task <- updateSubTask i toggleComplete <$> getCurrentTask state
    setCurrentTask task state

remove :: Stateful
remove state = do
    i <- getCurrentSubTask state
    task <- removeSubTask i <$> getCurrentTask state
    state' <- setCurrentTask task state
    setIndex state' i

insertMode :: Stateful
insertMode state = do
    i <- getCurrentSubTask state
    task <- getCurrentTask state
    n <- name <$> getSubTask i task
    case mode state of
        Modal (Detail (DetailItem i') _) -> Just state { mode = Modal (Detail (DetailItem i') (DetailInsert (textToField n))) }
        _ -> Nothing

editSummary :: Stateful
editSummary state = do
    summ <- summary <$> getCurrentTask state
    let summ' = fromMaybe "" summ
    return $ state { mode = Modal (Detail DetailDescription (DetailInsert (textToField summ'))) }

editDue :: Stateful
editDue state = do
    day <- due <$> getCurrentTask state
    let day' = maybe "" dayToOutput day
    return $ state { mode = Modal (Detail DetailDate (DetailInsert (textToField day'))) }

newItem :: Stateful
newItem state = do
    task <- addSubTask blankSubTask <$> getCurrentTask state
    setCurrentTask task state

-- list navigation
changeSubTask :: Int -> Stateful
changeSubTask inc state = do
    i <- (+ inc) <$> getCurrentSubTask state
    setIndex state i

nextSubTask :: Stateful
nextSubTask = changeSubTask 1

previousSubTask :: Stateful
previousSubTask = changeSubTask (-1)

lastSubTask :: Stateful
lastSubTask state = lastIndex state >>= setIndex state

lastIndex :: State -> Maybe Int
lastIndex state = (+ (-1)) . countSubTasks <$> getCurrentTask state

setIndex :: State -> Int -> Maybe State
setIndex state i = do
    lst <- lastIndex state
    m <- getCurrentMode state
    let newIndex | i > lst = lst
                 | i < 0 = 0
                 | otherwise = i
    return $ state { mode = Modal (Detail (DetailItem newIndex) m) }
