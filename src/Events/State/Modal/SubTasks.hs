module Events.State.Modal.SubTasks where

import Events.State.Types
import Events.State (getCurrentTask, setCurrentTask, mode)
import Data.Taskell.Task (updateSubTask, toggleComplete)

getCurrentSubTask :: State -> Maybe Int
getCurrentSubTask state = case mode state of
    Modal (SubTasks index _) -> Just index
    _ -> Nothing

setComplete :: Stateful
setComplete state = do
    index <- getCurrentSubTask state
    task <- getCurrentTask state
    updatedTask <- updateSubTask index toggleComplete task
    setCurrentTask updatedTask state
