{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Events.State (
    -- types
    State,
    Stateful,
    Pointer,
    InsertMode(..),
    Mode(..),
    ModalType(..),

    -- App
    continue,
    write,
    io,
    path,
    countCurrent,

    -- record accesors
    mode,
    current,
    lists,

    -- UI.Main
    normalise,

    -- Main
    create,

    -- Events.Actions.Normal
    quit,
    startEdit,
    startCreate,
    createListStart,
    editListStart,
    deleteCurrentList,
    clearItem,
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
    listLeft,
    listRight,
    undo,
    store,
    searchMode,

    -- Events.Actions.Search
    searchEntered,

    -- Events.Actions.Insert
    createList,
    removeBlank,
    newItem,
    normalMode,
    finishTask,
    finishListTitle,

    -- Events.Actions.Modal
    showHelp,
    showMoveTo,
    moveTo,
    getCurrentList,
    getCurrentTask,
    setCurrentTask
) where

import ClassyPrelude hiding (delete)

import Control.Lens ((&), (^.), (.~))

import Data.Char (digitToInt, ord)

import Data.Taskell.Task (Task, isBlank, name)
import Data.Taskell.List (List(), update, move, new, deleteTask, newAt, getTask, title)
import qualified Data.Taskell.Lists as Lists

import Events.State.Types
import UI.Field (getText, blankField, textToField)

create :: FilePath -> Lists.Lists -> State
create p ls = State {
    mode = Normal,
    lists = ls,
    history = [],
    current = (0, 0),
    path = p,
    io = Nothing
}

-- app state
quit :: Stateful
quit s = return $ s { mode = Shutdown }

continue :: State -> State
continue s = s { io = Nothing }

write :: Stateful
write s = return $ s { io = Just (lists s) }

store :: Stateful
store s = return $ s { history = (current s, lists s) : history s }

undo :: Stateful
undo s = return $ case history s of
    [] -> s
    ((c, l):xs) -> s {
        current = c,
        lists = l,
        history = xs
    }

-- createList
createList :: Stateful
createList s = return $ case mode s of
    Insert IList ICreate f -> updateListToLast . setLists s $ Lists.newList (getText f) $ lists s
    _ -> s

updateListToLast :: InternalStateful
updateListToLast s = setCurrentList s (length (lists s) - 1)

createListStart :: Stateful
createListStart s = return $ s { mode = Insert IList ICreate blankField }

-- editList
editListStart :: Stateful
editListStart s = do
    f <- textToField . (^. title) <$> getList s
    return $ s { mode = Insert IList IEdit f }

deleteCurrentList :: Stateful
deleteCurrentList s = return $ fixIndex $ setLists s $ Lists.delete (getCurrentList s) (lists s)

-- insert
getCurrentTask :: State -> Maybe Task
getCurrentTask s = getList s >>= getTask (getIndex s)

setCurrentTask :: Task -> Stateful
setCurrentTask task state = do
    list <- update (getIndex state) task <$> getList state
    return $ setList state list

setCurrentTaskText :: Text -> Stateful
setCurrentTaskText text state = do
    task <- getCurrentTask state
    setCurrentTask (task & name .~ text) state

startCreate :: Stateful
startCreate s = return $ s { mode = Insert ITask ICreate blankField }

startEdit :: Stateful
startEdit state = do
    field <- textToField . (^. name) <$> getCurrentTask state
    return state { mode = Insert ITask IEdit field }

finishTask :: Stateful
finishTask s = case mode s of
    Insert ITask iMode f -> setCurrentTaskText (getText f) $ s { mode = Insert ITask iMode blankField }
    _ -> return s

finishListTitle :: Stateful
finishListTitle s = case mode s of
    Insert IList iMode f -> setCurrentListTitle (getText f) $ s { mode = Insert IList iMode blankField }
    _ -> return s


normalMode :: Stateful
normalMode s = return $ s { mode = Normal }

addToListAt :: Int -> Stateful
addToListAt d s = do
    let i = getIndex s + d
    ls <- newAt i <$> getList s
    return . fixIndex $ setList (setIndex s i) ls

above :: Stateful
above = addToListAt 0

below :: Stateful
below = addToListAt 1

newItem :: Stateful
newItem s = do
    l <- new <$> getList s
    return . selectLast $ setList s l

clearItem :: Stateful
clearItem = setCurrentTaskText ""

bottom :: Stateful
bottom = return . selectLast

selectLast :: InternalStateful
selectLast s = setIndex s (countCurrent s - 1)

removeBlank :: Stateful
removeBlank s = do
    c <- getCurrentTask s
    (if isBlank c then delete else return) s

-- moving
up :: Stateful
up s = do
    l <- move (getIndex s) (-1) =<< getList s
    previous $ setList s l

down :: Stateful
down s = do
    l <- move (getIndex s) 1 =<< getList s
    next $ setList s l

move' :: Int -> State -> Maybe State
move' i s = do
    l <- Lists.changeList (current s) (lists s) i
    return . fixIndex $ setLists s l

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
    return . fixIndex . setList s $ deleteTask (getIndex s) ts

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
setList s ts = setLists s (Lists.updateLists (getCurrentList s) (lists s) ts)

setCurrentListTitle :: Text -> Stateful
setCurrentListTitle text state = do
    list <- getList state
    return . setList state $ list & title .~ text

setLists :: State -> Lists.Lists -> State
setLists s ts = s { lists = ts }

moveTo :: Char -> Stateful
moveTo char state = do
    let li = ord char - ord 'a'
        cur = getCurrentList state

    if li == cur || li < 0 || li >= length (lists state)
        then Nothing
        else do
            s <- move' (li - cur) state
            return . selectLast $ setCurrentList s li

-- move lists
listMove :: Int -> Stateful
listMove dir s = do
    let c = getCurrentList s
    let lists' = lists s
    if c + dir < 0 || c + dir >= length lists'
        then Nothing
        else do
            ls <- Lists.shiftBy c dir lists'
            let s' = fixIndex $ setCurrentList s (c + dir)
            return $ setLists s' ls

listLeft :: Stateful
listLeft = listMove (-1)

listRight :: Stateful
listRight = listMove 1

-- search
searchMode :: Stateful
searchMode s = return $ case mode s of
    Search _ field -> s { mode = Search True field }
    _ -> s { mode = Search True blankField }

searchEntered :: Stateful
searchEntered s = case mode s of
    Search _ field -> return $ s { mode = Search False field }
    _ -> Nothing

-- help
showHelp :: Stateful
showHelp s = return $ s { mode = Modal Help }

showMoveTo :: Stateful
showMoveTo s = return $ s { mode = Modal MoveTo }

-- view - maybe shouldn't be in here...
search :: State -> State
search s = case mode s of
    Search _ field -> fixIndex $ setLists s $ Lists.search (getText field) (lists s)
    _ -> s

newList :: State -> State
newList s = case mode s of
    Insert IList ICreate f ->
        let ls = lists s in
        fixIndex $ setCurrentList (setLists s (Lists.newList (getText f) ls)) (length ls)
    _ -> s

normalise :: State -> State
normalise = newList . search
