module Flow.State where

import Data.Taskell.Task (Task, backspace, append, characters)
import Data.Taskell.Tasks (Tasks(Tasks), update, move, new, deleteTask, getTask)
import qualified Data.Taskell.AllTasks as All
import qualified Data.Taskell.String as S

data Mode = Normal | Insert | CreateList | Shutdown deriving (Show)

data State = State {
    mode :: Mode,
    tasks :: All.AllTasks, 
    current :: (Int, Int),
    size :: (Int, Int),
    newList :: String
} deriving (Show)

create :: (Int, Int) -> All.AllTasks -> State
create size ts = State {
        mode = Normal,
        tasks = ts,
        current = (0, 0),
        size = size,
        newList = ""
    } 

type Stateful = State -> State

-- app state
quit :: Stateful
quit s = s { mode = Shutdown }

setSize :: Int -> Int -> Stateful
setSize w h s = s { size = (w, h) }

-- createList
getNewList :: State -> Maybe String
getNewList s = case mode s of
    CreateList -> Just $ newList s
    _ -> Nothing

createList :: Stateful
createList s = setTasks s'' ts
    where listName = newList s
          s' = s { newList = "" }
          ts = All.newList listName $ getTasks s'
          s'' = setCurrentList s' (length ts - 1)

createListStart :: Stateful
createListStart s = s { mode = CreateList }

createListFinish :: Stateful
createListFinish = finishInsert . createList

createListCancel :: Stateful 
createListCancel = finishInsert 

createListBS :: Stateful 
createListBS s = s { newList = S.backspace (newList s) }

createListChar :: Char -> Stateful 
createListChar c s = s { newList = newList s ++ [c] }

deleteCurrentList :: Stateful
deleteCurrentList s = fixIndex $ setTasks s $ All.delete (getCurrentList s) (getTasks s)

-- insert
startInsert :: Stateful
startInsert s = s { mode = Insert }

finishInsert :: Stateful
finishInsert s = s { mode = Normal }

newItem :: Stateful
newItem s = selectLast $ setList s $ new (getList s)

insertBS :: Stateful
insertBS = change backspace

insertCurrent :: Char -> Stateful
insertCurrent = change . append

change :: (Task -> Task) -> Stateful
change fn s = setList s $ update (getIndex s) fn $ getList s

selectLast :: Stateful
selectLast s = setIndex s (countCurrent s - 1)

-- moving
up :: Stateful
up s = previous $ setList s (m (getList s))
    where m = move (getIndex s) (-1)

down :: Stateful
down s = next $ setList s (m (getList s))
    where m = move (getIndex s) 1

move' :: Int -> Stateful
move' i s = fixIndex $ setTasks s $ All.changeList (current s) (getTasks s) i 

moveLeft :: Stateful
moveLeft = move' (-1) 

moveRight :: Stateful
moveRight = move' 1

-- removing
delete :: Stateful
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

next :: Stateful
next s = setIndex s i'
    where
        i = getIndex s
        c = countCurrent s
        i' = if i < (c - 1) then succ i else i

previous :: Stateful
previous s = setIndex s i'
    where i = getIndex s
          i' = if i > 0 then pred i else 0

left :: Stateful
left s = fixIndex $ setCurrentList s $ if l > 0 then pred l else 0
    where l = getCurrentList s

right :: Stateful
right s = fixIndex $ setCurrentList s $ if l < (c - 1) then succ l else l
    where l = getCurrentList s
          c = length (getTasks s)

fixIndex :: Stateful
fixIndex s = if getIndex s' > c then setIndex s' c' else s'
    where i = All.exists (getCurrentList s) (getTasks s)
          s' = if i then s else setCurrentList s (length (getTasks s) - 1)
          c = countCurrent s' - 1
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

getCurrentTask :: State -> Maybe Task
getCurrentTask s = getTask i l
    where l = getList s
          i = getIndex s

-- view
getCursor :: State -> Maybe (Int, Int, Int)
getCursor s = do
    t <- getCurrentTask s
    let l = characters t

    case mode s of
        Insert -> return (getCurrentList s, getIndex s, l)
        _ -> Nothing
