module Events.State.Modal.SubTasks where

import Data.Maybe (fromMaybe)
import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (SubTask, updateSubTask, toggleComplete, subTasks, addSubTask, blankSubTask, stAppend, stBackspace, countSubTasks, removeSubTask, stAppendByteString)
import Data.Sequence as S (adjust')
import Data.ByteString (ByteString)

showSubTasks :: Stateful
showSubTasks s = return $ s { mode = Modal (SubTasks index STNormal) }
    where index = fromMaybe 0 $ getCurrentSubTask s

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (SubTasks index _) -> Just index
    _ -> Nothing

getCurrentMode :: State -> Maybe SubTasksMode
getCurrentMode state = case mode state of
    Modal (SubTasks _ m) -> Just m
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
insertMode state = case mode state of
    Modal (SubTasks index _) -> Just state { mode = Modal (SubTasks index STInsert) }
    _ -> Nothing

newItem :: Stateful
newItem state = do
    task <- addSubTask blankSubTask <$> getCurrentTask state
    setCurrentTask task state

insertBS :: Stateful
insertBS = change stBackspace

insertCurrent :: Char -> Stateful
insertCurrent char = change (stAppend char)

insertByteString :: ByteString -> Stateful
insertByteString bs = change (stAppendByteString bs)

change :: (SubTask -> SubTask) -> State -> Maybe State
change fn state = do
    index <- getCurrentSubTask state
    task <- getCurrentTask state
    let sts = adjust' fn index $ subTasks task
    let updatedTask = task { subTasks = sts }
    setCurrentTask updatedTask state

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
