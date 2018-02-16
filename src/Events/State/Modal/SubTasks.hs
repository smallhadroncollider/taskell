module Events.State.Modal.SubTasks where

import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (updateSubTask, toggleComplete, subTasks)
import Data.Sequence as S (length)

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (SubTasks index _) -> Just index
    _ -> Nothing

setComplete :: Stateful
setComplete state = do
    index <- getCurrentSubTask state
    task <- updateSubTask index toggleComplete <$> getCurrentTask state
    setCurrentTask task state

-- list navigation
changeSubTask :: Int -> Stateful
changeSubTask inc state = do
    index <- (+ inc) <$> getCurrentSubTask state
    lst <- (+ (-1)) . S.length . subTasks <$> getCurrentTask state
    let newIndex | index > lst = lst
                 | index < 0 = 0
                 | otherwise = index
    return $ state { mode = Modal (SubTasks newIndex STNormal) }

nextSubTask :: Stateful
nextSubTask = changeSubTask 1

previousSubTask :: Stateful
previousSubTask = changeSubTask (-1)
