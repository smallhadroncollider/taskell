{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Taskell.List.Internal where

import ClassyPrelude

import Control.Lens (element, makeLenses, (%%~), (%~), (&), (.~), (^.), (^?))

import Data.Sequence as S (adjust', deleteAt, insertAt, update, (|>))

import qualified Data.Taskell.Seq  as S
import qualified Data.Taskell.Task as T (Task, Update, blank, clearDue, contains, due, duplicate)
import           Types             (TaskIndex (TaskIndex))

data List = List
    { _title :: Text
    , _tasks :: Seq T.Task
    } deriving (Show, Eq)

type Update = List -> List

-- create lenses
$(makeLenses ''List)

-- operations
create :: Text -> Seq T.Task -> List
create = List

empty :: Text -> List
empty text = List text ClassyPrelude.empty

new :: Update
new = append T.blank

count :: List -> Int
count = length . (^. tasks)

due :: List -> Seq (TaskIndex, T.Task)
due list = catMaybes (filt S.<#> (list ^. tasks))
  where
    filt int task = const (TaskIndex int, task) <$> task ^. T.due

clearDue :: TaskIndex -> Update
clearDue (TaskIndex int) = updateFn int T.clearDue

newAt :: Int -> Update
newAt idx = tasks %~ S.insertAt idx T.blank

duplicate :: Int -> List -> Maybe List
duplicate idx list = do
    task <- T.duplicate <$> getTask idx list
    pure $ list & tasks %~ S.insertAt idx task

append :: T.Task -> Update
append task = tasks %~ (|> task)

extract :: Int -> List -> Maybe (List, T.Task)
extract idx list = do
    (xs, x) <- S.extract idx (list ^. tasks)
    pure (list & tasks .~ xs, x)

updateFn :: Int -> T.Update -> Update
updateFn idx fn = tasks %~ adjust' fn idx

update :: Int -> T.Task -> Update
update idx task = tasks %~ S.update idx task

move :: Int -> Int -> Maybe Text -> List -> Maybe (List, Int)
move current dir term list =
    case term of
        Nothing -> (, bound (current + dir) list) <$> (list & tasks %%~ S.shiftBy current dir)
        Just _ -> do
            idx <- changeTask dir current term list
            (, idx) <$> (list & tasks %%~ S.shiftBy current (idx - current))

deleteTask :: Int -> Update
deleteTask idx = tasks %~ deleteAt idx

getTask :: Int -> List -> Maybe T.Task
getTask idx = (^? tasks . element idx)

searchFor :: Text -> Update
searchFor text = tasks %~ filter (T.contains text)

changeTask :: Int -> Int -> Maybe Text -> List -> Maybe Int
changeTask dir current term list = do
    let next = current + dir
    tsk <- getTask next list
    case term of
        Nothing -> Just next
        Just trm ->
            if T.contains trm tsk
                then Just next
                else changeTask dir next term list

nextTask :: Int -> Maybe Text -> List -> Int
nextTask idx text lst = fromMaybe idx $ changeTask 1 idx text lst

prevTask :: Int -> Maybe Text -> List -> Int
prevTask idx text lst = fromMaybe idx $ changeTask (-1) idx text lst

closest :: Int -> Int -> Int -> Int
closest current previous next =
    if (next - current) < (current - previous)
        then next
        else previous

bound :: Int -> List -> Int
bound idx lst
    | idx < 0 = 0
    | idx >= count lst = count lst - 1
    | otherwise = idx

nearest' :: Int -> Maybe Text -> List -> Maybe Int
nearest' current term lst = do
    let prev = changeTask (-1) current term lst
    let nxt = changeTask 1 current term lst
    let comp idx = Just $ maybe idx (closest current idx) nxt
    maybe nxt comp prev

nearest :: Int -> Maybe Text -> List -> Int
nearest current term lst = idx
  where
    near = fromMaybe (-1) $ nearest' current term lst
    idx =
        case term of
            Nothing  -> bound current lst
            Just txt -> maybe near (bool near current . T.contains txt) $ getTask current lst
