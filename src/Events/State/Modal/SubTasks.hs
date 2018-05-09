{-# LANGUAGE NoImplicitPrelude #-}
module Events.State.Modal.SubTasks where

import ClassyPrelude

import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (updateSubTask, toggleComplete, addSubTask, blankSubTask, countSubTasks, removeSubTask, setSubTaskName, name, getSubTask)
import UI.Field (Field, blankField, getText, textToField)

finishSubTask :: Stateful
finishSubTask state = do
    text <- getText <$> getField state
    i <- getCurrentSubTask state
    task <- updateSubTask i (setSubTaskName text) <$> getCurrentTask state
    setCurrentTask task $ state { mode = Modal (SubTasks i (STInsert blankField)) }

showSubTasks :: Stateful
showSubTasks s = do
    _ <- getCurrentTask s
    let i = fromMaybe 0 $ getCurrentSubTask s
    return $ s { mode = Modal (SubTasks i STNormal) }

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (SubTasks i _) -> Just i
    _ -> Nothing

getCurrentMode :: State -> Maybe SubTasksMode
getCurrentMode state = case mode state of
    Modal (SubTasks _ m) -> Just m
    _ -> Nothing

getField :: State -> Maybe Field
getField state = case mode state of
    Modal (SubTasks _ (STInsert f)) -> Just f
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
        Modal (SubTasks i' _) -> Just state { mode = Modal (SubTasks i' (STInsert (textToField n))) }
        _ -> Nothing

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
    return $ state { mode = Modal (SubTasks newIndex m) }
