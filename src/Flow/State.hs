module Flow.State where

import Data.Taskell.Task (Task, backspace, append)
import Data.Taskell.Tasks (Tasks(Tasks), update, move, new, deleteTask)
import qualified Data.Taskell.AllTasks as All (AllTasks, update, count, get)

data Mode = Command | Insert | Shutdown deriving (Show)

data State = State {
    mode :: Mode,
    tasks :: All.AllTasks, 
    current :: (Int, Int)
} deriving (Show)

create :: All.AllTasks -> State
create ts = State {
        mode = Command,
        tasks = ts,
        current = (0, 0)
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
newItem s = selectLast $ setList s $ new (getList s)

insertBS :: State -> State
insertBS = change backspace

insertCurrent :: Char -> State -> State
insertCurrent = change . append

change :: (Task -> Task) -> State -> State
change fn s = setList s $ update (getIndex s) fn $ getList s

selectLast :: State -> State
selectLast s = setIndex s (countCurrent s - 1)

-- moving
up :: State -> State
up s = previous $ setList s (m (getList s))
    where m = move (getIndex s) (-1)

down :: State -> State
down s = next $ setList s (m (getList s))
    where m = move (getIndex s) 1

-- removing
delete :: State -> State
delete s = fixIndex $ setList s $ deleteTask (getIndex s) ts
    where ts = getList s

-- list and index
countCurrent :: State -> Int
countCurrent s = All.count (getCurrentList s) (getTasks s)

setIndex :: State -> Int -> State
setIndex s i = s { current = (getCurrentList s, i) }

setCurrentList :: State -> Int -> State
setCurrentList s i = s { current = (i, getIndex s) }

getIndex :: State -> Int
getIndex = snd . current

next :: State -> State
next s = setIndex s i'
    where
        i = getIndex s
        c = countCurrent s
        i' = if i < (c - 1) then succ i else i

previous :: State -> State
previous s = setIndex s i'
    where i = getIndex s
          i' = if i > 0 then pred i else 0

left :: State -> State
left s = fixIndex $ setCurrentList s $ if l > 0 then pred l else 0
    where l = getCurrentList s

right :: State -> State
right s = fixIndex $ setCurrentList s $ if l < (c - 1) then succ l else l
    where l = getCurrentList s
          c = length (getTasks s)

fixIndex :: State -> State
fixIndex s = if getIndex s > c then setIndex s c' else s
    where c = countCurrent s - 1
          c' = if c < 0 then 0 else c

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . current

getList :: State -> Tasks
getList s = All.get (tasks s) (getCurrentList s)

setList :: State -> Tasks -> State
setList s ts = setTasks s (All.update (getCurrentList s) (tasks s) ts)

setTasks :: State -> All.AllTasks -> State
setTasks s ts = s { tasks = ts }

getTasks :: State -> All.AllTasks
getTasks = tasks
