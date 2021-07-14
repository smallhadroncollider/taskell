module Taskell.Events.State
    -- App
    ( continue
    , write
    , setTime
    , countCurrent
    , setHeight
    -- Taskell.UI.Main
    , normalise
    -- Main
    , create
    -- Taskell.Events.Actions.Normal
    , quit
    , startEdit
    , startCreate
    , createListStart
    , editListStart
    , deleteCurrentList
    , clearItem
    , clearDate
    , above
    , below
    , bottom
    , top
    , previous
    , duplicate
    , next
    , left
    , right
    , up
    , down
    , moveLeftTop
    , moveRightTop
    , moveLeftBottom
    , moveRightBottom
    , moveToLastBottom
    , moveToLastTop
    , delete
    , selectList
    , listLeft
    , listRight
    , undo
    , redo
    , store
    , searchMode
    , clearSearch
    , appendSearch
    -- Taskell.Events.Actions.Insert
    , createList
    , removeBlank
    , newItem
    , normalMode
    , finishTask
    , finishListTitle
    -- Taskell.Events.Actions.Modal
    , showHelp
    , showMoveTo
    , moveTo
    , getCurrentList
    , getCurrentTask
    , setCurrentTask
    ) where

import ClassyPrelude hiding (delete)

import Control.Lens ((%~), (&), (.~), (?~), (^.))

import Data.Char       (digitToInt, ord)
import Data.Text       (strip)
import Data.Time.Zones (TZ)

import qualified Taskell.Data.List  as L (List, deleteTask, duplicate, getTask, move, nearest, new,
                                          newAt, nextTask, prevTask, title, update)
import qualified Taskell.Data.Lists as Lists
import           Taskell.Data.Task  (Task, isBlank, name)
import           Taskell.Types

import qualified Taskell.Events.State.History    as History (redo, store, undo)
import           Taskell.Events.State.Types
import           Taskell.Events.State.Types.Mode (InsertMode (..), InsertType (..), ModalType (..),
                                                  Mode (..))
import           Taskell.UI.Draw.Field           (Field, blankField, getText, textToField)

type InternalStateful = State -> State

create :: TZ -> UTCTime -> FilePath -> Lists.Lists -> State
create tz t p ls =
    State
    { _mode = Normal
    , _history = fresh ls
    , _path = p
    , _io = Nothing
    , _height = 0
    , _searchTerm = Nothing
    , _time = t
    , _timeZone = tz
    }

-- app state
quit :: Stateful
quit = pure . (mode .~ Shutdown)

continue :: State -> State
continue = io .~ Nothing

store :: Stateful
store state = pure $ state & history %~ History.store

undo :: Stateful
undo state = pure $ state & history %~ History.undo

redo :: Stateful
redo state = pure $ state & history %~ History.redo

setTime :: UTCTime -> State -> State
setTime t = time .~ t

write :: Stateful
write state = pure $ state & (io ?~ (state ^. lists))

-- createList
createList :: Stateful
createList state =
    pure $
    case state ^. mode of
        Insert IList ICreate f ->
            updateListToLast . setLists state $ Lists.newList (getText f) $ state ^. lists
        _ -> state

updateListToLast :: InternalStateful
updateListToLast state = setCurrentList state (length (state ^. lists) - 1)

createListStart :: Stateful
createListStart = pure . (mode .~ Insert IList ICreate blankField)

-- editList
editListStart :: Stateful
editListStart state = do
    f <- textToField . (^. L.title) <$> getList state
    pure $ state & mode .~ Insert IList IEdit f

deleteCurrentList :: Stateful
deleteCurrentList state =
    pure . fixIndex . setLists state $ Lists.delete (getCurrentList state) (state ^. lists)

-- insert
getCurrentTask :: State -> Maybe Task
getCurrentTask state = getList state >>= L.getTask (getIndex state)

setCurrentTask :: Task -> Stateful
setCurrentTask task state = setList state . L.update (getIndex state) task <$> getList state

setCurrentTaskText :: Text -> Stateful
setCurrentTaskText text state =
    flip setCurrentTask state =<< (name .~ strip text) <$> getCurrentTask state

startCreate :: Stateful
startCreate = pure . (mode .~ Insert ITask ICreate blankField)

startEdit :: Stateful
startEdit state = do
    field <- textToField . (^. name) <$> getCurrentTask state
    pure $ state & mode .~ Insert ITask IEdit field

finishTask :: Stateful
finishTask state =
    case state ^. mode of
        Insert ITask iMode f ->
            setCurrentTaskText (getText f) $ state & (mode .~ Insert ITask iMode blankField)
        _ -> pure state

finishListTitle :: Stateful
finishListTitle state =
    case state ^. mode of
        Insert IList iMode f ->
            setCurrentListTitle (getText f) $ state & (mode .~ Insert IList iMode blankField)
        _ -> pure state

normalMode :: Stateful
normalMode = pure . (mode .~ Normal)

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
duplicate state = setList state <$> (L.duplicate (getIndex state) =<< getList state)

clearItem :: Stateful
clearItem = setCurrentTaskText ""

clearDate :: Stateful
clearDate state = pure $ state & lists .~ Lists.clearDue (state ^. current) (state ^. lists)

bottom :: Stateful
bottom = pure . selectLast

selectLast :: InternalStateful
selectLast state = setIndex state (countCurrent state - 1)

top :: Stateful
top = pure . selectFirst

selectFirst :: InternalStateful
selectFirst state = setIndex state (0)

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
    pure $ setIndex (setList state lst) idx

up :: Stateful
up = moveVertical (-1)

down :: Stateful
down = moveVertical 1

moveHorizontal :: Int -> Lists.ListPosition -> State -> Maybe State
moveHorizontal idx pos state =
    fixIndex . setLists state <$> Lists.changeList pos (state ^. current) (state ^. lists) idx

moveLeftBottom :: Stateful
moveLeftBottom = moveHorizontal (-1) Lists.Bottom

moveRightBottom :: Stateful
moveRightBottom = moveHorizontal 1 Lists.Bottom

moveLeftTop :: Stateful
moveLeftTop = moveHorizontal (-1) Lists.Top

moveRightTop :: Stateful
moveRightTop = moveHorizontal 1 Lists.Top

moveToLast :: Lists.ListPosition -> Stateful
moveToLast pos state =
  if idx == cur
      then pure state
      else moveHorizontal (idx - cur) pos state
  where
    idx = length (state ^. lists) - 1
    cur = getCurrentList state

moveToLastBottom :: Stateful
moveToLastBottom = moveToLast Lists.Bottom

moveToLastTop :: Stateful
moveToLastTop = moveToLast Lists.Top

selectList :: Char -> Stateful
selectList idx state =
    pure $
    (if exists
         then current .~ (ListIndex list, TaskIndex 0)
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
setIndex state idx = state & current .~ (ListIndex (getCurrentList state), TaskIndex idx)

setCurrentList :: State -> Int -> State
setCurrentList state idx = state & current .~ (ListIndex idx, TaskIndex (getIndex state))

getIndex :: State -> Int
getIndex = showTaskIndex . snd . (^. current)

changeTask :: (Int -> Maybe Text -> L.List -> Int) -> Stateful
changeTask fn state = do
    list <- getList state
    let idx = getIndex state
    let term = getText <$> state ^. searchTerm
    pure $ setIndex state (fn idx term list)

next :: Stateful
next = changeTask L.nextTask

previous :: Stateful
previous = changeTask L.prevTask

left :: Stateful
left state =
    pure . fixIndex . setCurrentList state $
    if list > 0
        then pred list
        else 0
  where
    list = getCurrentList state

right :: Stateful
right state =
    pure . fixIndex . setCurrentList state $
    if list < (count - 1)
        then succ list
        else list
  where
    list = getCurrentList state
    count = length (state ^. lists)

fixListIndex :: InternalStateful
fixListIndex state =
    if listIdx
        then state
        else setCurrentList state (length lists' - 1)
  where
    lists' = state ^. lists
    listIdx = Lists.exists (getCurrentList state) lists'

fixIndex :: InternalStateful
fixIndex state =
    case getList state of
        Just list -> setIndex state (L.nearest idx trm list)
        Nothing   -> fixListIndex state
  where
    trm = getText <$> state ^. searchTerm
    idx = getIndex state

-- tasks
getCurrentList :: State -> Int
getCurrentList = showListIndex . fst . (^. current)

getList :: State -> Maybe L.List
getList state = Lists.get (state ^. lists) (getCurrentList state)

setList :: State -> L.List -> State
setList state list = setLists state (Lists.updateLists (getCurrentList state) list (state ^. lists))

setCurrentListTitle :: Text -> Stateful
setCurrentListTitle text state = setList state . (L.title .~ text) <$> getList state

setLists :: State -> Lists.Lists -> State
setLists state lists' = state & lists .~ lists'

moveTo' :: Int -> Stateful
moveTo' li state = do
    let cur = getCurrentList state
    if li == cur || li < 0 || li >= length (state ^. lists)
        then Nothing
        else do
            s <- moveHorizontal (li - cur) Lists.Bottom state
            pure . selectLast $ setCurrentList s li

moveTo :: Char -> Stateful
moveTo char = moveTo' (ord char - ord 'a')

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
    sTerm = pure (fromMaybe blankField (state ^. searchTerm))

clearSearch :: Stateful
clearSearch state = pure $ state & searchTerm .~ Nothing

appendSearch :: (Field -> Field) -> Stateful
appendSearch genField state = do
    let field = fromMaybe blankField (state ^. searchTerm)
    pure . fixIndex $ state & searchTerm .~ pure (genField field)

-- help
showHelp :: Stateful
showHelp = pure . (mode .~ Modal Help)

showMoveTo :: Stateful
showMoveTo state = const (state & mode .~ Modal MoveTo) <$> getCurrentTask state

-- view
setHeight :: Int -> State -> State
setHeight = (.~) height

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
