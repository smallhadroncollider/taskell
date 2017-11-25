module Flow.State (
    -- types
    State,
    Stateful,
    Mode(..),
    
    -- record accesors
    mode,
    size,
    current,
    tasks,

    -- UI.Main
    getCursor,
    getNewList,
    
    -- Main
    create,
    
    -- Flow.Actions.Normal 
    quit,
    startInsert,
    createListStart,
    deleteCurrentList,
    previous,
    next,
    left,
    right,
    up,
    down,
    moveLeft,
    moveRight,
    delete,
    setSize,

    -- Flow.Actions.CreateList
    createListFinish,
    createListCancel,
    createListBS,
    createListChar,

    -- Flow.Actions.Insert
    newItem,
    finishInsert,
    insertBS,
    insertCurrent
) where

import Data.Taskell.Task (Task, backspace, append, characters)
import Data.Taskell.List (List(List), update, move, new, deleteTask, getTask)
import qualified Data.Taskell.Lists as Lists
import qualified Data.Taskell.String as S

data Mode = Normal | Insert | CreateList | Shutdown deriving (Show)

data State = State {
    mode :: Mode,
    tasks :: Lists.Lists, 
    current :: (Int, Int),
    size :: (Int, Int),
    newList :: String
} deriving (Show)

create :: (Int, Int) -> Lists.Lists -> State
create size ts = State {
        mode = Normal,
        tasks = ts,
        current = (0, 0),
        size = size,
        newList = ""
    } 

type Stateful = State -> Maybe State
type InternalStateful = State -> State 

-- app state
quit :: Stateful
quit s = return $ s { mode = Shutdown }

setSize :: Int -> Int -> Stateful
setSize w h s = return $ s { size = (w, h) }

-- createList
getNewList :: State -> Maybe String
getNewList s = case mode s of
    CreateList -> Just $ newList s
    _ -> Nothing

createList :: InternalStateful
createList s = setTasks s'' ts
    where listName = newList s
          s' = s { newList = "" }
          ts = Lists.newList listName $ getTasks s'
          s'' = setCurrentList s' (length ts - 1)

createListStart :: Stateful
createListStart s = return $ s { mode = CreateList }

createListFinish :: Stateful
createListFinish = finishInsert . createList

createListCancel :: Stateful 
createListCancel = finishInsert 

createListBS :: Stateful 
createListBS s = return $ s { newList = S.backspace (newList s) }

createListChar :: Char -> Stateful 
createListChar c s = return $ s { newList = newList s ++ [c] }

deleteCurrentList :: Stateful
deleteCurrentList s = return $ fixIndex $ setTasks s $ Lists.delete (getCurrentList s) (getTasks s)

-- insert
startInsert :: Stateful
startInsert s = return $ s { mode = Insert }

finishInsert :: Stateful
finishInsert s = return $ s { mode = Normal }

newItem :: Stateful
newItem s = return $ selectLast $ setList s $ new (getList s)

insertBS :: Stateful
insertBS = return . change backspace

insertCurrent :: Char -> Stateful
insertCurrent char = return . change (append char)

change :: (Task -> Task) -> InternalStateful
change fn s = setList s $ update (getIndex s) fn $ getList s

selectLast :: InternalStateful
selectLast s = setIndex s (countCurrent s - 1)

-- moving
up :: Stateful
up s = previous $ setList s (m (getList s))
    where m = move (getIndex s) (-1)

down :: Stateful
down s = next $ setList s (m (getList s))
    where m = move (getIndex s) 1

move' :: Int -> InternalStateful
move' i s = fixIndex $ setTasks s $ Lists.changeList (current s) (getTasks s) i 

moveLeft :: Stateful
moveLeft = return . move' (-1)

moveRight :: Stateful
moveRight = return . move' 1

-- removing
delete :: Stateful
delete s = return $ fixIndex $ setList s $ deleteTask (getIndex s) ts
    where ts = getList s

-- list and index
countCurrent :: State -> Int
countCurrent s = Lists.count (getCurrentList s) (getTasks s)

setIndex :: State -> Int -> State
setIndex s i = s { current = (getCurrentList s, i) }

setCurrentList :: State -> Int -> State
setCurrentList s i = s { current = (i, getIndex s) }

getIndex :: State -> Int
getIndex = snd . current

next :: Stateful
next s = return $ setIndex s i'
    where
        i = getIndex s
        c = countCurrent s
        i' = if i < (c - 1) then succ i else i

previous :: Stateful
previous s = return $ setIndex s i'
    where i = getIndex s
          i' = if i > 0 then pred i else 0

left :: Stateful
left s = return $ fixIndex $ setCurrentList s $ if l > 0 then pred l else 0
    where l = getCurrentList s

right :: Stateful
right s = return $ fixIndex $ setCurrentList s $ if l < (c - 1) then succ l else l
    where l = getCurrentList s
          c = length (getTasks s)

fixIndex :: InternalStateful
fixIndex s = if getIndex s' > c then setIndex s' c' else s'
    where i = Lists.exists (getCurrentList s) (getTasks s)
          s' = if i then s else setCurrentList s (length (getTasks s) - 1)
          c = countCurrent s' - 1
          c' = if c < 0 then 0 else c

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . current

getList :: State -> List
getList s = Lists.get (tasks s) (getCurrentList s)

setList :: State -> List -> State
setList s ts = setTasks s (Lists.update (getCurrentList s) (tasks s) ts)

setTasks :: State -> Lists.Lists -> State
setTasks s ts = s { tasks = ts }

getTasks :: State -> Lists.Lists
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
