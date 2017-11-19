module Flow.State where

import Data.Taskell.Task (Tasks, completed, empty)
import Data.Sequence (mapWithIndex) 

data State = State {
    running :: Bool, -- whether the app is running
    tasks :: Tasks, -- the tasks
    current :: (Int, Int) -- the list and index
} deriving (Show)

initial :: State
initial = (State {
        tasks = empty,
        current = (0, 0),
        running = True
    }) 

-- list and index
setIndex :: State -> Int -> State
setIndex s i = s { current = (getList s, i) }

setList :: State -> Int -> State
setList s l = s { current = (l, getIndex s) }

getIndex :: State -> Int
getIndex s = snd $ current s

getList :: State -> Int
getList s = fst $ current s

shiftIndex :: Int -> State -> State
shiftIndex i s = setIndex s x 
    where
        inc = (getIndex s) + i
        x = inc `mod` count s

next :: State -> State
next = shiftIndex 1 

previous :: State -> State
previous = shiftIndex (-1)

switch :: State -> State
switch s = setList s l
    where l = if getList s == 1 then 0 else 1

-- length
count :: State -> Int
count = length . tasks

-- not terribly efficient
-- goes over ever item
mapCompleted :: State -> Tasks
mapCompleted s = mapWithIndex set (tasks s)
    where
        i = getIndex s
        set = \cur t -> if cur == i then t { completed = (not (completed t)) } else t

setCompleted :: State -> State
setCompleted s = s { tasks = (mapCompleted s) }

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = ts }

quit :: State -> State
quit s = s { running = False }
