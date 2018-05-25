{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Lists where

import ClassyPrelude hiding (empty)

import Data.Sequence as S ((!?), (|>), update, deleteAt)

import Data.Taskell.List as L (List(..), empty, extract, append, searchFor, count)
import Data.Taskell.Task (Task)
import qualified Data.Taskell.Seq as S

type Lists = Seq List

initial :: Lists
initial = fromList [empty "To Do", empty "Done"]

updateLists :: Int -> Lists -> List -> Lists
updateLists i ls l = S.update i l ls

count :: Int -> Lists -> Int
count i ts = case ts !? i of
    Just (List _ ts') -> length ts'
    Nothing -> 0

get :: Lists -> Int -> Maybe List
get = (!?)

changeList :: (Int, Int) -> Lists -> Int -> Maybe Lists
changeList (list, i) ts dir = do
    let next = list + dir
    a <- ts !? list -- get current list
    b <- ts !? next -- get next list
    (a', task) <- extract i a -- extract selected task
    let b' = append b task -- add selected task to next list
    let list' = updateLists list ts a' -- update extracted list
    return $ updateLists next list' b' -- update next list

newList :: Text -> Lists -> Lists
newList s ts = ts |> empty s

delete :: Int -> Lists -> Lists
delete = deleteAt

exists :: Int -> Lists -> Bool
exists i ts = isJust $ ts !? i

shiftBy :: Int -> Int -> Lists -> Maybe Lists
shiftBy = S.shiftBy

search :: Text -> Lists -> Lists
search s ls = searchFor s <$> ls

appendToLast :: Task -> Lists -> Lists
appendToLast t ls = fromMaybe ls $ do
    let i = length ls - 1
    l <- ls !? i
    let l' = append l t
    return $ updateLists i ls l'

analyse :: Text -> Lists -> Text
analyse filepath ls = concat [
        filepath , "\n"
      , "Lists: ", tshow $ length ls, "\n"
      , "Tasks: ", tshow $ foldl' (+) 0 (L.count <$> ls)
    ]
