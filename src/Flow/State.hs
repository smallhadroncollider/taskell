module Flow.State (
    -- types
    State,
    Stateful,
    Pointer,
    Size,
    Mode(..),
    
    -- record accesors
    mode,
    size,
    current,
    lists,

    -- UI.Main
    showCursor,
    newList,
    
    -- Main
    create,
    
    -- Flow.Actions.Normal 
    quit,
    startInsert,
    createListStart,
    deleteCurrentList,
    above,
    below,
    bottom,
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
    listLeft,
    listRight,

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
import Data.Taskell.List (List(List), update, move, new, deleteTask, getTask, newAt)
import qualified Data.Taskell.Lists as Lists
import qualified Data.Taskell.String as S
import Data.Char (digitToInt)

data Mode = Normal | Insert | CreateList String | Shutdown deriving (Show)

type Size = (Int, Int)
type Pointer = (Int, Int)

data State = State {
    mode :: Mode,
    lists :: Lists.Lists, 
    current :: Pointer,
    size :: Size
} deriving (Show)

create :: Size -> Lists.Lists -> State
create size ls = State {
        mode = Normal,
        lists = ls,
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
createList :: InternalStateful
createList s =  case mode s of
    CreateList n -> updateListToLast . setLists s $ Lists.newList n $ lists s
    _ -> s 

updateListToLast :: InternalStateful
updateListToLast s = setCurrentList s (length (lists s) - 1)

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
deleteCurrentList s = return $ fixIndex $ setLists s $ Lists.delete (getCurrentList s) (lists s)

-- insert
startInsert :: Stateful
startInsert s = return $ s { mode = Insert }

finishInsert :: Stateful
finishInsert s = return $ s { mode = Normal }

addToListAt :: Int -> Stateful
addToListAt d s = do
    l <- getList s
    let i = getIndex s + d
    let ls = newAt i l
    return $ setList (setIndex s i) ls

above :: Stateful
above = addToListAt 0 

below :: Stateful
below = addToListAt 1 

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

bottom :: Stateful
bottom = return . selectLast

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
    l <- Lists.changeList (current s) (lists s) i 
    return $ fixIndex $ setLists s l

moveLeft :: Stateful
moveLeft = move' (-1)

moveRight :: Stateful
moveRight = move' 1

selectList :: Char -> Stateful
selectList i s = return $ if e then s { current = (list, 0) } else s
    where list = digitToInt i - 1
          e = Lists.exists list (lists s)

-- removing
delete :: Stateful
delete s = do
    ts <- getList s
    return $ fixIndex $ setList s $ deleteTask (getIndex s) ts

-- list and index
countCurrent :: State -> Int
countCurrent s = Lists.count (getCurrentList s) (lists s)

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
          c = length (lists s)

fixIndex :: InternalStateful
fixIndex s = if getIndex s' > c then setIndex s' c' else s'
    where i = Lists.exists (getCurrentList s) (lists s)
          s' = if i then s else setCurrentList s (length (lists s) - 1)
          c = countCurrent s' - 1
          c' = if c < 0 then 0 else c

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . current

getList :: State -> Maybe List
getList s = Lists.get (lists s) (getCurrentList s)

setList :: State -> List -> State
setList s ts = setLists s (Lists.update (getCurrentList s) (lists s) ts)

setLists :: State -> Lists.Lists -> State
setLists s ts = s { lists = ts }

getCurrentTask :: State -> Maybe Task
getCurrentTask s = do
    l <- getList s
    let i = getIndex s
    getTask i l

-- move lists
listMove :: Int -> Stateful
listMove dir s = do
    let ls = lists s
    let c = getCurrentList s
    ls' <- Lists.shiftBy c dir ls
    let s' = fixIndex $ setCurrentList s (c + dir)
    return $ setLists s' ls'

listLeft :: Stateful
listLeft = listMove (-1)

listRight :: Stateful
listRight = listMove 1

-- view
showCursor :: State -> Bool 
showCursor s = case mode s of
    Insert -> True 
    CreateList n -> True 
    _ -> False

newList :: State -> State 
newList s = case mode s of
    CreateList t -> fixIndex $ setCurrentList (setLists s (Lists.newList t ls)) (length ls)
    _ -> s 
    where ls = lists s
