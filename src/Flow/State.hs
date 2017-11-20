module Flow.State where

import Data.Taskell.Task (Tasks, split, empty)
import Data.Sequence ((><), mapWithIndex) 

data State = State {
    running :: Bool, -- whether the app is running
    tasks :: (Tasks, Tasks), -- the todo and done tasks 
    current :: (Int, Int) -- the list and index
} deriving (Show)

initial :: State
initial = (State {
        tasks = (empty, empty),
        current = (0, 0),
        running = True
    }) 

-- list and index
-- setIndex :: State -> Int -> State
-- setIndex s i = s { current = (getList s, i) }
--
-- setList :: State -> Int -> State
-- setList s l = s { current = (l, getIndex s) }
--
-- getIndex :: State -> Int
-- getIndex s = snd $ current s
--
-- getList :: State -> Int
-- getList s = fst $ current s
--
-- shiftIndex :: Int -> State -> State
-- shiftIndex i s = setIndex s x 
--     where
--         inc = (getIndex s) + i
--         x = inc `mod` count s
--
-- next :: State -> State
-- next = shiftIndex 1 
--
-- previous :: State -> State
-- previous = shiftIndex (-1)
--
-- switch :: State -> State
-- switch s = setList s l
--     where l = if getList s == 1 then 0 else 1

-- tasks
getDone :: State -> Tasks
getDone = snd . tasks

getToDo :: State -> Tasks
getToDo = fst . tasks

setDone :: State -> Tasks -> State
setDone s ts = s { tasks = (getToDo s, ts) }

setToDo :: State -> Tasks -> State
setToDo s ts = s { tasks = (ts, getDone s) }

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = split ts }

getTasks :: State -> Tasks
getTasks s = fst (tasks s) >< snd (tasks s)

-- app state
quit :: State -> State
quit s = s { running = False }
