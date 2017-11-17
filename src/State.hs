module State where

import Task (Tasks, completed)
import Data.Sequence (mapWithIndex) 

data State = State {
    tasks :: Tasks,
    current :: Int,
    showCompleted :: Bool,
    running :: Bool
} deriving (Show)

type StateUpdate = State -> State

setCurrent :: State -> Int -> State
setCurrent s i = s { current = i }

count :: State -> Int
count = length . tasks

index :: Int -> State -> State
index i s = s { current = x }
    where
        inc = ((current s) + i)
        x = inc `mod` count s

next :: StateUpdate
next = index 1 

previous :: StateUpdate
previous = index (-1)

-- not terribly efficient
-- goes over ever item
mapCompleted :: State -> Tasks
mapCompleted s = mapWithIndex set (tasks s)
    where
        i = current s
        set = \cur t -> if cur == i then t { completed = (not (completed t)) } else t

setCompleted :: StateUpdate
setCompleted s = s { tasks = (mapCompleted s) }

toggleShowCompleted :: StateUpdate
toggleShowCompleted s = s { showCompleted = (not (showCompleted s)) }

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = ts }

quit :: StateUpdate
quit s = s { running = False }
