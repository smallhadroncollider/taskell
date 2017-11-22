module Flow.State where

import Data.Maybe (fromMaybe)
import Data.Sequence ((><), (|>), deleteAt) 
import Data.Map.Strict ((!), (!?), insert, keys)

import Data.Taskell.Task
import Data.Taskell.Tasks
import Data.Taskell.AllTasks

data Mode = Command | Insert | Shutdown deriving (Show)

data State = State {
    mode :: Mode,
    tasks :: AllTasks, 
    current :: (String, Int)
} deriving (Show)

create :: AllTasks -> State
create ts = State {
        mode = Command,
        tasks = ts,
        current = (k, 0)
    } 
    where k = head $ keys ts 

-- app state
quit :: State -> State
quit s = s { mode = Shutdown }

-- insert
startInsert :: State -> State
startInsert s = s { mode = Insert }

finishInsert :: State -> State
finishInsert s = s { mode = Command }

newItem :: State -> State
newItem s = setList s (getList s |> blank)

insertBS :: State -> State
insertBS = change backspace

insertCurrent :: Char -> State -> State
insertCurrent = change . append

change :: (Task -> Task) -> State -> State
change fn s = setList s $ update (getIndex s) fn $ getList s

-- moving
up :: State -> State
up s = previous $ setList s (m (getList s))
    where m = move (getIndex s) (-1)

down :: State -> State
down s = next $ setList s (m (getList s))
    where m = move (getIndex s) 1

-- removing
delete :: State -> State
delete s = setList s (deleteAt (getIndex s) (getList s))

-- list and index
count :: String -> State -> Int
count k s = case getTasks s !? k of
    Just ts -> length ts
    Nothing -> 0

countCurrent :: State -> Int
countCurrent s = count (getCurrentList s) s

setIndex :: State -> Int -> State
setIndex s i = s { current = (getCurrentList s, i) }

setCurrentList :: State -> String -> State
setCurrentList s l = s { current = (l, getIndex s) }

getIndex :: State -> Int
getIndex = snd . current

next :: State -> State
next s = setIndex s i'
    where
        list = getCurrentList s
        i = getIndex s
        c = count list s
        i' = if i < (c - 1) then succ i else i

previous :: State -> State
previous s = setIndex s i'
    where i = getIndex s
          i' = if i > 0 then pred i else 0

switch :: State -> State
switch = id

fixIndex :: State -> State
fixIndex s = if getIndex s > c then setIndex s c' else s
    where c = countCurrent s - 1
          c' = if c < 0 then 0 else c

-- tasks
getCurrentList :: State -> String
getCurrentList = fst . current

getList :: State -> Tasks
getList s = tasks s ! getCurrentList s -- not safe

setList :: State -> Tasks -> State
setList s ts = setTasks s $ insert (getCurrentList s) ts (tasks s)

setTasks :: State -> AllTasks -> State
setTasks s ts = s { tasks = ts }

getTasks :: State -> AllTasks
getTasks = tasks
