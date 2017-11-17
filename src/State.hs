module State where

import Task (Tasks)

data State = State {
    tasks :: Tasks,
    current :: Int,
    running :: Bool
} deriving (Show)

setCurrent :: State -> Int -> State
setCurrent s i = s { current = i }

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = ts }

quit :: State -> State
quit s = s { running = False }
