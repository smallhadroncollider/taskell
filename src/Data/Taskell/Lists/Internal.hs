{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Lists.Internal where

import ClassyPrelude

import Control.Lens  ((^.))
import Data.Sequence as S (deleteAt, update, (!?), (|>))

import qualified Data.Taskell.List as L (List, append, count, due, empty, extract, searchFor)
import qualified Data.Taskell.Seq  as S
import qualified Data.Taskell.Task as T (Task, due)

type Lists = Seq L.List

type Update = Lists -> Lists

initial :: Lists
initial = fromList [L.empty "To Do", L.empty "Done"]

updateLists :: Int -> L.List -> Update
updateLists = S.update

count :: Int -> Lists -> Int
count idx tasks = maybe 0 L.count (tasks !? idx)

due :: Lists -> Seq T.Task
due = sortOn (^. T.due) . foldl' (<>) empty . (L.due <$>)

get :: Lists -> Int -> Maybe L.List
get = (!?)

changeList :: (Int, Int) -> Lists -> Int -> Maybe Lists
changeList (list, idx) tasks dir = do
    let next = list + dir
    (from, task) <- L.extract idx =<< tasks !? list -- extract current task
    to <- L.append task <$> tasks !? next -- get next list and append task
    pure . updateLists next to $ updateLists list from tasks -- update lists

newList :: Text -> Update
newList title = (|> L.empty title)

delete :: Int -> Update
delete = deleteAt

exists :: Int -> Lists -> Bool
exists idx tasks = isJust $ tasks !? idx

shiftBy :: Int -> Int -> Lists -> Maybe Lists
shiftBy = S.shiftBy

search :: Text -> Update
search text = (L.searchFor text <$>)

appendToLast :: T.Task -> Update
appendToLast task lists =
    fromMaybe lists $ do
        let idx = length lists - 1
        list <- L.append task <$> lists !? idx
        pure $ updateLists idx list lists

analyse :: Text -> Lists -> Text
analyse filepath lists =
    concat
        [ filepath
        , "\n"
        , "Lists: "
        , tshow $ length lists
        , "\n"
        , "Tasks: "
        , tshow $ foldl' (+) 0 (L.count <$> lists)
        ]
