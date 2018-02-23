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
    searchBS,
    searchChar,
    searchEntered,

    -- Events.Actions.CreateList
    createList,
    createListBS,
    createListChar,

    -- Events.Actions.EditList
    editListBS,
    editListChar,

    -- Events.Actions.Create/Edit
    removeBlank,
    newItem,
    normalMode,
    insertBS,
    insertCurrent,

    -- Events.Actions.Modal
    showHelp,
    showMoveTo,
    moveTo,
    getCurrentTask,
    setCurrentTask
) where

import Data.Text (snoc, null)
import Data.Taskell.Task (Task, backspace, append, clear, isBlank)
import Data.Taskell.List (List(), updateFn, update, move, new, deleteTask, newAt, title, updateTitle, getTask)
import qualified Data.Taskell.Lists as Lists
import qualified Data.Taskell.Text as T
import Data.Char (digitToInt, ord)

import Events.State.Types

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
    Insert (CreateList n) -> updateListToLast . setLists s $ Lists.newList n $ lists s
    _ -> s

updateListToLast :: InternalStateful
updateListToLast s = setCurrentList s (length (lists s) - 1)

createListStart :: Stateful
createListStart s = return $ s { mode = Insert (CreateList "") }

createListBS :: Stateful
createListBS s = case mode s of
    Insert (CreateList n) -> return $ s { mode = Insert (CreateList (T.backspace n)) }
    _ -> Nothing

createListChar :: Char -> Stateful
createListChar c s = case mode s of
    Insert (CreateList n) -> return $ s { mode = Insert (CreateList (Data.Text.snoc n c)) }
    _ -> Nothing

-- editList
editListStart :: Stateful
editListStart s = return $ s { mode = Insert EditList }

editListBS :: Stateful
editListBS s = case mode s of
    Insert EditList -> do
        l <- getList s
        let t = T.backspace (title l)
        return $ setList s $ updateTitle l t
    _ -> Nothing

editListChar :: Char -> Stateful
editListChar c s = case mode s of
    Insert EditList -> do
        l <- getList s
        let t = Data.Text.snoc (title l) c
        return $ setList s $ updateTitle l t
    _ -> Nothing

deleteCurrentList :: Stateful
deleteCurrentList s = return $ fixIndex $ setLists s $ Lists.delete (getCurrentList s) (lists s)

-- insert
getCurrentTask :: State -> Maybe Task
getCurrentTask s = do
    l <- getList s
    getTask (getIndex s) l

setCurrentTask :: Task -> Stateful
setCurrentTask task state = do
    list <- update (getIndex state) task <$> getList state
    return $ setList state list

startCreate :: Stateful
startCreate s = return $ s { mode = Insert CreateTask }

startEdit :: Stateful
startEdit s = return $ s { mode = Insert EditTask }

normalMode :: Stateful
normalMode s = return $ s { mode = Normal }

addToListAt :: Int -> Stateful
addToListAt d s = do
    l <- getList s
    let i = getIndex s + d
    let ls = newAt i l
    return $ fixIndex $ setList (setIndex s i) ls

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
insertCurrent char = change (Data.Taskell.Task.append char)

change :: (Task -> Task) -> State -> Maybe State
change fn s = do
    l <- updateFn (getIndex s) fn <$> getList s
    return $ setList s l

clearItem :: Stateful
clearItem = change clear

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
setList s ts = setLists s (Lists.updateLists (getCurrentList s) (lists s) ts)

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
    let ls = lists s
    let c = getCurrentList s
    ls' <- Lists.shiftBy c dir ls
    let s' = fixIndex $ setCurrentList s (c + dir)
    return $ setLists s' ls'

listLeft :: Stateful
listLeft = listMove (-1)

listRight :: Stateful
listRight = listMove 1

-- search
searchMode :: Stateful
searchMode s = return $ case mode s of
    Search _ term -> s { mode = Search True term }
    _ -> s { mode = Search True "" }

searchEntered :: Stateful
searchEntered s = case mode s of
    Search _ term -> return $ s { mode = Search False term }
    _ -> Nothing

searchBS :: Stateful
searchBS s = case mode s of
    Search ent term -> return $
        if Data.Text.null term
            then s { mode = Normal }
            else s { mode = Search ent (T.backspace term) }
    _ -> Nothing

searchChar :: Char -> Stateful
searchChar c s = case mode s of
    Search ent term -> return $ s { mode = Search ent (Data.Text.snoc term c) }
    _ -> Nothing

-- help
showHelp :: Stateful
showHelp s = return $ s { mode = Modal Help }

showMoveTo :: Stateful
showMoveTo s = return $ s { mode = Modal MoveTo }

-- view - maybe shouldn't be in here...
search :: State -> State
search s = case mode s of
    Search _ term -> fixIndex $ setLists s $ Lists.search term (lists s)
    _ -> s

newList :: State -> State
newList s = case mode s of
    Insert (CreateList t) -> let ls = lists s in
                               fixIndex $ setCurrentList (setLists s (Lists.newList t ls)) (length ls)
    _ -> s

normalise :: State -> State
normalise = newList . search
