module Flow.State where

import Data.Taskell.Task (Tasks, completed, empty)
import Data.Sequence (mapWithIndex) 

data State = State {
    tasks :: Tasks,
    current :: Int,
    running :: Bool
} deriving (Show)

initial :: State
initial = (State {
        tasks = empty,
        current = 0,
        running = True
    }) 

setCurrent :: State -> Int -> State
setCurrent s i = s { current = i }

count :: State -> Int
count = length . tasks

index :: Int -> State -> State
index i s = s { current = x }
    where
        inc = ((current s) + i)
        x = inc `mod` count s

next :: State -> State
next = index 1 

previous :: State -> State
previous = index (-1)

-- not terribly efficient
-- goes over ever item
mapCompleted :: State -> Tasks
mapCompleted s = mapWithIndex set (tasks s)
    where
        i = current s
        set = \cur t -> if cur == i then t { completed = (not (completed t)) } else t

setCompleted :: State -> State
setCompleted s = s { tasks = (mapCompleted s) }

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = ts }

quit :: State -> State
quit s = s { running = False }
