{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Events.State
    -- App
    ( continue
    , write
    , countCurrent
    , setHeight
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
    , duplicate
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
    , clearSearch
    , appendSearch
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

import qualified Data.Taskell.List  as L (List, deleteTask, duplicate, getTask, move, nearest, new,
                                          newAt, nextTask, prevTask, title, update)
import qualified Data.Taskell.Lists as Lists
import           Data.Taskell.Task  (Task, isBlank, name)

import Events.State.Types
import Events.State.Types.Mode (InsertMode (..), InsertType (..), ModalType (..), Mode (..))
import UI.Field                (Field, blankField, getText, textToField)

type InternalStateful = State -> State

create :: FilePath -> Lists.Lists -> State
create p ls =
    State
    { _mode = Normal
    , _lists = ls
    , _history = []
    , _current = (0, 0)
    , _path = p
    , _io = Nothing
    , _height = 0
    , _searchTerm = Nothing
    }

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
    f <- textToField . (^. L.title) <$> getList state
    pure $ state & mode .~ Insert IList IEdit f

deleteCurrentList :: Stateful
deleteCurrentList state =
    Just . fixIndex . setLists state $ Lists.delete (getCurrentList state) (state ^. lists)

-- insert
getCurrentTask :: State -> Maybe Task
getCurrentTask state = getList state >>= L.getTask (getIndex state)

setCurrentTask :: Task -> Stateful
setCurrentTask task state = setList state . L.update (getIndex state) task <$> getList state

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
    fixIndex . setList (setIndex state idx) . L.newAt idx <$> getList state

above :: Stateful
above = addToListAt 0

below :: Stateful
below = addToListAt 1

newItem :: Stateful
newItem state = selectLast . setList state . L.new <$> getList state

duplicate :: Stateful
duplicate state = do
    let idx = getIndex state
    list <- getList state
    setList state <$> L.duplicate idx list

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
--
moveVertical :: Int -> Stateful
moveVertical dir state = do
    (lst, idx) <- L.move (getIndex state) dir (getText <$> state ^. searchTerm) =<< getList state
    Just $ setIndex (setList state lst) idx

up :: Stateful
up = moveVertical (-1)

down :: Stateful
down = moveVertical 1

moveHorizontal :: Int -> State -> Maybe State
moveHorizontal idx state =
    fixIndex . setLists state <$> Lists.changeList (state ^. current) (state ^. lists) idx

moveLeft :: Stateful
moveLeft = moveHorizontal (-1)

moveRight :: Stateful
moveRight = moveHorizontal 1

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
delete state = fixIndex . setList state . L.deleteTask (getIndex state) <$> getList state

-- list and index
countCurrent :: State -> Int
countCurrent state = Lists.count (getCurrentList state) (state ^. lists)

setIndex :: State -> Int -> State
setIndex state idx = state & current .~ (getCurrentList state, idx)

setCurrentList :: State -> Int -> State
setCurrentList state idx = state & current .~ (idx, getIndex state)

getIndex :: State -> Int
getIndex = snd . (^. current)

changeTask :: (Int -> Maybe Text -> L.List -> Int) -> Stateful
changeTask fn state = do
    list <- getList state
    let idx = getIndex state
    let term = getText <$> state ^. searchTerm
    Just $ setIndex state (fn idx term list)

next :: Stateful
next = changeTask L.nextTask

previous :: Stateful
previous = changeTask L.prevTask

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
    case getList state of
        Just list -> setIndex state (L.nearest idx trm list)
        Nothing   -> state
  where
    trm = getText <$> state ^. searchTerm
    idx = getIndex state

-- tasks
getCurrentList :: State -> Int
getCurrentList = fst . (^. current)

getList :: State -> Maybe L.List
getList state = Lists.get (state ^. lists) (getCurrentList state)

setList :: State -> L.List -> State
setList state list = setLists state (Lists.updateLists (getCurrentList state) list (state ^. lists))

setCurrentListTitle :: Text -> Stateful
setCurrentListTitle text state = setList state . (L.title .~ text) <$> getList state

setLists :: State -> Lists.Lists -> State
setLists state lists' = state & lists .~ lists'

moveTo :: Char -> Stateful
moveTo char state = do
    let li = ord char - ord 'a'
        cur = getCurrentList state
    if li == cur || li < 0 || li >= length (state ^. lists)
        then Nothing
        else do
            s <- moveHorizontal (li - cur) state
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
searchMode state = pure . fixIndex $ (state & mode .~ Search) & searchTerm .~ sTerm
  where
    sTerm = Just (fromMaybe blankField (state ^. searchTerm))

clearSearch :: Stateful
clearSearch state = pure $ state & searchTerm .~ Nothing

appendSearch :: (Field -> Field) -> Stateful
appendSearch genField state = do
    let field = fromMaybe blankField (state ^. searchTerm)
    pure . fixIndex $ state & searchTerm .~ Just (genField field)

-- help
showHelp :: Stateful
showHelp = Just . (mode .~ Modal Help)

showMoveTo :: Stateful
showMoveTo = Just . (mode .~ Modal MoveTo)

-- view
setHeight :: Int -> State -> State
setHeight i = height .~ i

-- more view - maybe shouldn't be in here...
newList :: State -> State
newList state =
    case state ^. mode of
        Insert IList ICreate f ->
            let ls = state ^. lists
            in fixIndex $ setCurrentList (setLists state (Lists.newList (getText f) ls)) (length ls)
        _ -> state

normalise :: State -> State
normalise = newList
