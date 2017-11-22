module Flow.State where

import Data.Taskell.Task
import Data.Maybe (fromMaybe)
import Data.Sequence ((><), (|>), deleteAt) 

data CurrentList = ToDo | Done deriving (Show, Eq)
data Mode = Command | Insert | Shutdown deriving (Show);

data State = State {
    mode :: Mode,
    tasks :: (Tasks, Tasks), -- the todo and done tasks 
    current :: (CurrentList, Int) -- the list and index
} deriving (Show)

initial :: State
initial = State {
        mode = Command,
        tasks = (empty, empty),
        current = (ToDo, 0)
    } 

-- app state
quit :: State -> State
quit s = s { mode = Shutdown }

-- insert
startInsert :: State -> State
startInsert s = s { mode = Insert }

finishInsert :: State -> State
finishInsert s = s { mode = Command }

newItem :: State -> State
newItem s = setToDo indexed (getToDo indexed |> blank)
    where listed = setList s ToDo 
          indexed = setIndex listed (count ToDo listed)

change :: (Task -> Task) -> State -> State
change fn s = case getList s of
    ToDo -> setToDo s $ update' $ getToDo s
    Done -> setDone s $ update' $ getDone s
    where update' = update (getIndex s) fn

insertBS :: State -> State
insertBS = change backspace

insertCurrent :: Char -> State -> State
insertCurrent = change . append

-- moving
up :: State -> State
up s = previous $ case getList s of
    ToDo -> setToDo s (m (getToDo s))
    Done -> setDone s (m (getDone s))
    where m = move (getIndex s) (-1)

down :: State -> State
down s = next $ case getList s of
    ToDo -> setToDo s (m (getToDo s))
    Done -> setDone s (m (getDone s))
    where m = move (getIndex s) 1

-- removing
delete :: State -> State
delete s = case getList s of
    ToDo -> setToDo s (deleteAt (getIndex s) (getToDo s))
    Done -> setDone s (deleteAt (getIndex s) (getDone s))

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

next :: State -> State
next s = setIndex s i'
    where
        list = getList s
        i = getIndex s
        c = count list s
        i' = if i < (c - 1) then succ i else i

previous :: State -> State
previous s = setIndex s i'
    where i = getIndex s
          i' = if i > 0 then pred i else 0

switch :: State -> State
switch s = fixIndex $ case getList s of
    ToDo -> setList s Done
    Done -> setList s ToDo

fixIndex :: State -> State
fixIndex s = if getIndex s > c then setIndex s c' else s
    where c = countCurrent s - 1
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
getTasks s = uncurry (><) (tasks s)

-- completed
toggle :: (State -> Tasks, State -> Tasks) -> (State -> Tasks -> State, State -> Tasks -> State) -> State -> Maybe State
toggle (fromGet, toGet) (fromSet, toSet) s = do
    (removed, current) <- extract (getIndex s) (fromGet s)
    let updated = toSet s (toGet s |> swap current)
    let final = fromSet updated removed
    return $ fixIndex final

toggleCompleted' :: State -> Maybe State
toggleCompleted' s = case getList s of
    ToDo -> toggle (getToDo, getDone) (setToDo, setDone) s
    Done -> toggle (getDone, getToDo) (setDone, setToDo) s

toggleCompleted :: State -> State
toggleCompleted s = fromMaybe s (toggleCompleted' s)
