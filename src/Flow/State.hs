module Flow.State where

import Prelude hiding (take, drop)
import Data.Taskell.Task (Tasks, Task, extract, split, empty, swap)
import Data.Sequence ((><), (|>)) 

data CurrentList = ToDo | Done deriving (Show, Eq)

data State = State {
    running :: Bool, -- whether the app is running
    tasks :: (Tasks, Tasks), -- the todo and done tasks 
    current :: (CurrentList, Int) -- the list and index
} deriving (Show)

initial :: State
initial = (State {
        tasks = (empty, empty),
        current = (ToDo, 0),
        running = True
    }) 

-- list and index
count :: CurrentList -> State -> Int
count ToDo = length . getToDo
count Done = length . getDone

countCurrent :: State -> Int
countCurrent s = count (getList s) s

setIndex :: State -> Int -> State
setIndex s i = s { current = (getList s, i) }

setList :: State -> CurrentList -> State
setList s l = s { current = (l, getIndex s) }

getIndex :: State -> Int
getIndex = snd . current

getList :: State -> CurrentList
getList = fst . current

shiftIndex :: (Int -> Int) -> State -> State
shiftIndex fn s = setIndex s x 
    where
        list = getList s
        inc = fn $ getIndex s
        x = mod inc $ count list s

next :: State -> State
next = shiftIndex succ

previous :: State -> State
previous = shiftIndex pred

switch :: State -> State
switch s = fixIndex $ case getList s of
    ToDo -> setList s Done
    Done -> setList s ToDo

fixIndex :: State -> State
fixIndex s = if getIndex s > c then setIndex s c' else s
    where c = (countCurrent s) - 1
          c' = if c < 0 then 0 else c

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

-- completed
-- all rather grim... probably needs a monad
toggleMaybe (removed, Just current) s toSet toGet fromSet = fixIndex final
    where updated = toSet s ((toGet s) |> (swap current))
          final = fromSet updated removed

toggleMaybe (_, Nothing) s _ _ _ = s

toggle s (fromGet, toGet) (fromSet, toSet) = toggleMaybe extracted s toSet toGet fromSet
    where extracted = extract (getIndex s) (fromGet s)

toggleCompleted :: State -> State
toggleCompleted s = case getList s of
    ToDo -> toggle s (getToDo, getDone) (setToDo, setDone)
    Done -> toggle s (getDone, getToDo) (setDone, setToDo)

-- app state
quit :: State -> State
quit s = s { running = False }
