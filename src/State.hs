module State where

import Task (Tasks)

data State = State {
    tasks :: Tasks,
    current :: Int,
    running :: Bool
} deriving (Show)

type StateUpdate = State -> State

setCurrent :: State -> Int -> State
setCurrent s i = s { current = i }

count :: State -> Int
count = length . tasks

index :: Int -> State -> State
index i s = s { current = x }
    where x = ((current s) + i) `mod` count s

next :: StateUpdate
next = index 1 

previous :: StateUpdate
previous = index (-1)

setTasks :: State -> Tasks -> State
setTasks s ts = s { tasks = ts }

quit :: StateUpdate
quit s = s { running = False }
