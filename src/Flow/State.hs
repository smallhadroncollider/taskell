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
    selectList,
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
import Data.Char (digitToInt)

data Mode = Normal | Insert | CreateList String | Shutdown deriving (Show)

type Size = (Int, Int)
type Pointer = (Int, Int)

data State = State {
    mode :: Mode,
    tasks :: Lists.Lists, 
    current :: Pointer,
    size :: Size
} deriving (Show)

create :: Size -> Lists.Lists -> State
create size ts = State {
        mode = Normal,
        tasks = ts,
        current = (0, 0),
        size = size
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
    CreateList n -> Just n 
    _ -> Nothing

createList :: InternalStateful
createList s =  case mode s of
    CreateList n -> updateListToLast . setTasks s $ Lists.newList n $ tasks s
    _ -> s 

updateListToLast :: InternalStateful
updateListToLast s = setCurrentList s (length (tasks s) - 1)

createListStart :: Stateful
createListStart s = return $ s { mode = CreateList "" }

createListFinish :: Stateful
createListFinish = finishInsert . createList

createListCancel :: Stateful 
createListCancel = finishInsert 

createListBS :: Stateful 
createListBS s = case mode s of
    CreateList n -> return $ s { mode = CreateList (S.backspace n) }
    _ -> Nothing 

createListChar :: Char -> Stateful 
createListChar c s = case mode s of
    CreateList n -> return $ s { mode = CreateList (n ++ [c]) }
    _ -> Nothing

deleteCurrentList :: Stateful
deleteCurrentList s = return $ fixIndex $ setTasks s $ Lists.delete (getCurrentList s) (tasks s)

-- insert
startInsert :: Stateful
startInsert s = return $ s { mode = Insert }

finishInsert :: Stateful
finishInsert s = return $ s { mode = Normal }

newItem :: Stateful
newItem s = do
    l <- getList s
    return $ selectLast $ setList s (new l)

insertBS :: Stateful
insertBS = change backspace

insertCurrent :: Char -> Stateful
insertCurrent char = change (append char)

change :: (Task -> Task) -> State -> Maybe State 
change fn s = do
    l <- getList s
    l' <- update (getIndex s) fn l
    return $ setList s l'

selectLast :: InternalStateful
selectLast s = setIndex s (countCurrent s - 1)

-- moving
up :: Stateful
up s = do
    l <- getList s
    l' <- m l
    previous $ setList s l' 
    where m = move (getIndex s) (-1)

down :: Stateful
down s = do
    l <- getList s
    l' <- m l
    next $ setList s l' 
    where m = move (getIndex s) 1

move' :: Int -> State -> Maybe State 
move' i s = do
    l <- Lists.changeList (current s) (tasks s) i 
    return $ fixIndex $ setTasks s l

moveLeft :: Stateful
moveLeft = move' (-1)

moveRight :: Stateful
moveRight = move' 1

selectList :: Char -> Stateful
selectList i s = return $ if e then s { current = (list, 0) } else s
    where list = digitToInt i - 1
          e = Lists.exists list (tasks s)

-- removing
delete :: Stateful
delete s = do
    ts <- getList s
    return $ fixIndex $ setList s $ deleteTask (getIndex s) ts

-- list and index
countCurrent :: State -> Int
countCurrent s = Lists.count (getCurrentList s) (tasks s)

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
          c = length (tasks s)

fixIndex :: InternalStateful
fixIndex s = if getIndex s' > c then setIndex s' c' else s'
    where i = Lists.exists (getCurrentList s) (tasks s)
          s' = if i then s else setCurrentList s (length (tasks s) - 1)
          c = countCurrent s' - 1
          c' = if c < 0 then 0 else c

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . current

getList :: State -> Maybe List
getList s = Lists.get (tasks s) (getCurrentList s)

setList :: State -> List -> State
setList s ts = setTasks s (Lists.update (getCurrentList s) (tasks s) ts)

setTasks :: State -> Lists.Lists -> State
setTasks s ts = s { tasks = ts }

getCurrentTask :: State -> Maybe Task
getCurrentTask s = do
    l <- getList s
    let i = getIndex s
    getTask i l

-- view
getCursor :: State -> Maybe (Int, Int, Int)
getCursor s = do
    t <- getCurrentTask s
    let l = characters t

    case mode s of
        Insert -> return (getCurrentList s, getIndex s, l)
        _ -> Nothing
