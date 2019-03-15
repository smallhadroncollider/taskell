{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.State
    -- App
    ( continue
    , write
    , countCurrent
    -- UI.Main
    , normalise
    -- Main
    , create
    -- Events.Actions.Normal
    , quit
    , startEdit
    , startCreate
    , createListStart
    , editListStart
    , deleteCurrentList
    , clearItem
    , above
    , below
    , bottom
    , previous
    , next
    , left
    , right
    , up
    , down
    , moveLeft
    , moveRight
    , delete
    , selectList
    , listLeft
    , listRight
    , undo
    , store
    , searchMode
    -- Events.Actions.Search
    , searchEntered
    -- Events.Actions.Insert
    , createList
    , removeBlank
    , newItem
    , normalMode
    , finishTask
    , finishListTitle
    -- Events.Actions.Modal
    , showHelp
    , showMoveTo
    , moveTo
    , getCurrentList
    , getCurrentTask
    , setCurrentTask
    ) where

import ClassyPrelude hiding (delete)

import Control.Lens ((&), (.~), (^.))

import Data.Char (digitToInt, ord)

import           Data.Taskell.List  (List, deleteTask, getTask, move, new, newAt, title, update)
import qualified Data.Taskell.Lists as Lists
import           Data.Taskell.Task  (Task, isBlank, name)

import Events.State.Types
import Events.State.Types.Mode (InsertMode (..), InsertType (..), ModalType (..), Mode (..))
import UI.Field                (blankField, getText, textToField)

type InternalStateful = State -> State

create :: FilePath -> Lists.Lists -> State
create p ls =
    State {_mode = Normal, _lists = ls, _history = [], _current = (0, 0), _path = p, _io = Nothing}

-- app state
quit :: Stateful
quit = Just . (mode .~ Shutdown)

continue :: State -> State
continue = io .~ Nothing

write :: Stateful
write state = Just $ state & io .~ Just (state ^. lists)

store :: Stateful
store state = Just $ state & history .~ (state ^. current, state ^. lists) : state ^. history

undo :: Stateful
undo state =
    Just $
    case state ^. history of
        []          -> state
        ((c, l):xs) -> state & current .~ c & lists .~ l & history .~ xs

-- createList
createList :: Stateful
createList state =
    Just $
    case state ^. mode of
        Insert IList ICreate f ->
            updateListToLast . setLists state $ Lists.newList (getText f) $ state ^. lists
        _ -> state

updateListToLast :: InternalStateful
updateListToLast state = setCurrentList state (length (state ^. lists) - 1)

createListStart :: Stateful
createListStart = Just . (mode .~ Insert IList ICreate blankField)

-- editList
editListStart :: Stateful
editListStart state = do
    f <- textToField . (^. title) <$> getList state
    pure $ state & mode .~ Insert IList IEdit f

deleteCurrentList :: Stateful
deleteCurrentList state =
    Just . fixIndex . setLists state $ Lists.delete (getCurrentList state) (state ^. lists)

-- insert
getCurrentTask :: State -> Maybe Task
getCurrentTask state = getList state >>= getTask (getIndex state)

setCurrentTask :: Task -> Stateful
setCurrentTask task state = setList state . update (getIndex state) task <$> getList state

setCurrentTaskText :: Text -> Stateful
setCurrentTaskText text state =
    flip setCurrentTask state =<< (name .~ text) <$> getCurrentTask state

startCreate :: Stateful
startCreate = Just . (mode .~ Insert ITask ICreate blankField)

startEdit :: Stateful
startEdit state = do
    field <- textToField . (^. name) <$> getCurrentTask state
    pure $ state & mode .~ Insert ITask IEdit field

finishTask :: Stateful
finishTask state =
    case state ^. mode of
        Insert ITask iMode f ->
            setCurrentTaskText (getText f) $ state & (mode .~ Insert ITask iMode blankField)
        _ -> Just state

finishListTitle :: Stateful
finishListTitle state =
    case state ^. mode of
        Insert IList iMode f ->
            setCurrentListTitle (getText f) $ state & (mode .~ Insert IList iMode blankField)
        _ -> Just state

normalMode :: Stateful
normalMode = Just . (mode .~ Normal)

addToListAt :: Int -> Stateful
addToListAt offset state = do
    let idx = getIndex state + offset
    fixIndex . setList (setIndex state idx) . newAt idx <$> getList state

above :: Stateful
above = addToListAt 0

below :: Stateful
below = addToListAt 1

newItem :: Stateful
newItem state = selectLast . setList state . new <$> getList state

clearItem :: Stateful
clearItem = setCurrentTaskText ""

bottom :: Stateful
bottom = pure . selectLast

selectLast :: InternalStateful
selectLast state = setIndex state (countCurrent state - 1)

removeBlank :: Stateful
removeBlank state = do
    currentTask <- getCurrentTask state
    (if isBlank currentTask
         then delete
         else pure)
        state

-- moving
up :: Stateful
up state = previous =<< setList state <$> (move (getIndex state) (-1) =<< getList state)

down :: Stateful
down state = next =<< setList state <$> (move (getIndex state) 1 =<< getList state)

move' :: Int -> State -> Maybe State
move' idx state =
    fixIndex . setLists state <$> Lists.changeList (state ^. current) (state ^. lists) idx

moveLeft :: Stateful
moveLeft = move' (-1)

moveRight :: Stateful
moveRight = move' 1

selectList :: Char -> Stateful
selectList idx state =
    Just $
    (if exists
         then current .~ (list, 0)
         else id)
        state
  where
    list = digitToInt idx - 1
    exists = Lists.exists list (state ^. lists)

-- removing
delete :: Stateful
delete state = fixIndex . setList state . deleteTask (getIndex state) <$> getList state

-- list and index
countCurrent :: State -> Int
countCurrent state = Lists.count (getCurrentList state) (state ^. lists)

setIndex :: State -> Int -> State
setIndex state idx = state & current .~ (getCurrentList state, idx)

setCurrentList :: State -> Int -> State
setCurrentList state idx = state & current .~ (idx, getIndex state)

getIndex :: State -> Int
getIndex = snd . (^. current)

next :: Stateful
next state = Just $ setIndex state idx'
  where
    idx = getIndex state
    count = countCurrent state
    idx' =
        if idx < (count - 1)
            then succ idx
            else idx

previous :: Stateful
previous state = Just $ setIndex state idx'
  where
    idx = getIndex state
    idx' =
        if idx > 0
            then pred idx
            else 0

left :: Stateful
left state =
    Just . fixIndex . setCurrentList state $
    if list > 0
        then pred list
        else 0
  where
    list = getCurrentList state

right :: Stateful
right state =
    Just . fixIndex . setCurrentList state $
    if list < (count - 1)
        then succ list
        else list
  where
    list = getCurrentList state
    count = length (state ^. lists)

fixIndex :: InternalStateful
fixIndex state =
    if getIndex state' > count
        then setIndex state' count'
        else state'
  where
    lists' = state ^. lists
    idx = Lists.exists (getCurrentList state) lists'
    state' =
        if idx
            then state
            else setCurrentList state (length lists' - 1)
    count = countCurrent state' - 1
    count' =
        if count < 0
            then 0
            else count

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . (^. current)

getList :: State -> Maybe List
getList state = Lists.get (state ^. lists) (getCurrentList state)

setList :: State -> List -> State
setList state list = setLists state (Lists.updateLists (getCurrentList state) list (state ^. lists))

setCurrentListTitle :: Text -> Stateful
setCurrentListTitle text state = setList state . (title .~ text) <$> getList state

setLists :: State -> Lists.Lists -> State
setLists state lists' = state & lists .~ lists'

moveTo :: Char -> Stateful
moveTo char state = do
    let li = ord char - ord 'a'
        cur = getCurrentList state
    if li == cur || li < 0 || li >= length (state ^. lists)
        then Nothing
        else do
            s <- move' (li - cur) state
            pure . selectLast $ setCurrentList s li

-- move lists
listMove :: Int -> Stateful
listMove offset state = do
    let currentList = getCurrentList state
    let lists' = state ^. lists
    if currentList + offset < 0 || currentList + offset >= length lists'
        then Nothing
        else do
            let state' = fixIndex $ setCurrentList state (currentList + offset)
            setLists state' <$> Lists.shiftBy currentList offset lists'

listLeft :: Stateful
listLeft = listMove (-1)

listRight :: Stateful
listRight = listMove 1

-- search
searchMode :: Stateful
searchMode state =
    Just $
    case state ^. mode of
        Search _ field -> state & mode .~ Search True field
        _              -> state & mode .~ Search True blankField

searchEntered :: Stateful
searchEntered state =
    case state ^. mode of
        Search _ field -> Just $ state & mode .~ Search False field
        _              -> Nothing

-- help
showHelp :: Stateful
showHelp = Just . (mode .~ Modal Help)

showMoveTo :: Stateful
showMoveTo = Just . (mode .~ Modal MoveTo)

-- view - maybe shouldn't be in here...
search :: State -> State
search state =
    case state ^. mode of
        Search _ field -> fixIndex . setLists state $ Lists.search (getText field) (state ^. lists)
        _ -> state

newList :: State -> State
newList state =
    case state ^. mode of
        Insert IList ICreate f ->
            let ls = state ^. lists
            in fixIndex $ setCurrentList (setLists state (Lists.newList (getText f) ls)) (length ls)
        _ -> state

normalise :: State -> State
normalise = newList . search
