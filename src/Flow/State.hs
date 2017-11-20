module Flow.State where

import Prelude hiding (take, drop)
import Data.Taskell.Task (Tasks, extract, split, empty, swap)
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

-- app state
quit :: State -> State
quit s = s { running = False }

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
        c = count list s
        x = if c /= 0 then inc `mod` c else 0 

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
toggle :: (State -> Tasks, State -> Tasks) -> (State -> Tasks -> State, State -> Tasks -> State) -> State -> Maybe State
toggle (fromGet, toGet) (fromSet, toSet) s = do
    (removed, current) <- extract (getIndex s) (fromGet s)
    let updated = toSet s ((toGet s) |> (swap current))
    let final = fromSet updated removed
    return $ fixIndex final

toggleCompleted' :: State -> Maybe State
toggleCompleted' s = case getList s of
    ToDo -> toggle (getToDo, getDone) (setToDo, setDone) s
    Done -> toggle (getDone, getToDo) (setDone, setToDo) s

toggleCompleted :: State -> State
toggleCompleted s = maybe s id (toggleCompleted' s)
