module Events.State.Modal.SubTasks where

import Data.Maybe (fromMaybe)
import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (updateSubTask, toggleComplete, addSubTask, blankSubTask, countSubTasks, removeSubTask, setSubTaskName, name, getSubTask)
import UI.Field (Field, blankField, getText, textToField)

finishSubTask :: Stateful
finishSubTask state = do
    text <- getText <$> getField state
    index <- getCurrentSubTask state
    task <- updateSubTask index (setSubTaskName text) <$> getCurrentTask state
    setCurrentTask task $ state { mode = Modal (SubTasks index (STInsert blankField)) }

showSubTasks :: Stateful
showSubTasks s = do
    getCurrentTask s
    let index = fromMaybe 0 $ getCurrentSubTask s
    return $ s { mode = Modal (SubTasks index STNormal) }

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (SubTasks index _) -> Just index
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
    index <- getCurrentSubTask state
    task <- updateSubTask index toggleComplete <$> getCurrentTask state
    setCurrentTask task state

remove :: Stateful
remove state = do
    index <- getCurrentSubTask state
    task <- removeSubTask index <$> getCurrentTask state
    state' <- setCurrentTask task state
    setIndex state' index

insertMode :: Stateful
insertMode state = do
    index <- getCurrentSubTask state
    task <- getCurrentTask state
    n <- name <$> getSubTask index task
    case mode state of
        Modal (SubTasks i _) -> Just state { mode = Modal (SubTasks i (STInsert (textToField n))) }
        _ -> Nothing

newItem :: Stateful
newItem state = do
    task <- addSubTask blankSubTask <$> getCurrentTask state
    setCurrentTask task state

-- list navigation
changeSubTask :: Int -> Stateful
changeSubTask inc state = do
    index <- (+ inc) <$> getCurrentSubTask state
    setIndex state index

nextSubTask :: Stateful
nextSubTask = changeSubTask 1

previousSubTask :: Stateful
previousSubTask = changeSubTask (-1)

lastSubTask :: Stateful
lastSubTask state = lastIndex state >>= setIndex state

lastIndex :: State -> Maybe Int
lastIndex state = (+ (-1)) . countSubTasks <$> getCurrentTask state

setIndex :: State -> Int -> Maybe State
setIndex state index = do
    lst <- lastIndex state
    m <- getCurrentMode state
    let newIndex | index > lst = lst
                 | index < 0 = 0
                 | otherwise = index
    return $ state { mode = Modal (SubTasks newIndex m) }
