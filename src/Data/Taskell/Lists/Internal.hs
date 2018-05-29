{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Lists.Internal where

import ClassyPrelude hiding (empty)

import Data.Sequence as S ((!?), (|>), update, deleteAt)

import Data.Taskell.List as L (List, empty, extract, append, searchFor, count)
import Data.Taskell.Task (Task)
import qualified Data.Taskell.Seq as S

type Lists = Seq List
type Update = Lists -> Lists

initial :: Lists
initial = fromList [empty "To Do", empty "Done"]

updateLists :: Int -> List -> Update
updateLists = S.update

count :: Int -> Lists -> Int
count idx tasks = maybe 0 L.count (tasks !? idx)

get :: Lists -> Int -> Maybe List
get = (!?)

changeList :: (Int, Int) -> Lists -> Int -> Maybe Lists
changeList (list, idx) tasks dir = do
    let next = list + dir
    a <- tasks !? list -- get current list
    b <- tasks !? next -- get next list
    (a', task) <- extract idx a -- extract selected task
    let b' = append task b -- add selected task to next list
    let list' = updateLists list a' tasks -- update extracted list
    return $ updateLists next b' list'-- update next list

newList :: Text -> Update
newList title = (|> empty title)

delete :: Int -> Update
delete = deleteAt

exists :: Int -> Lists -> Bool
exists idx tasks = isJust $ tasks !? idx

shiftBy :: Int -> Int -> Lists -> Maybe Lists
shiftBy = S.shiftBy

search :: Text -> Update
search text = (searchFor text <$>)

appendToLast :: Task -> Update
appendToLast task lists = fromMaybe lists $ do
    let idx = length lists - 1
    list <- lists !? idx
    let list' = append task list
    return $ updateLists idx list' lists

analyse :: Text -> Lists -> Text
analyse filepath lists = concat [
        filepath , "\n"
      , "Lists: ", tshow $ length lists, "\n"
      , "Tasks: ", tshow $ foldl' (+) 0 (L.count <$> lists)
    ]
