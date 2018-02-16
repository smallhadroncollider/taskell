{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Lists where

import Prelude hiding (length)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Sequence as S (Seq, fromList, (!?), (|>), deleteAt, length, update)
import qualified Data.Taskell.Seq as S
import Data.Taskell.Task (Task)
import Data.Taskell.List (List(..), empty, extract, append, searchFor)

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
    let b' = append task b -- add selected task to next list
    let list' = updateLists list ts a' -- update extracted list
    return $ updateLists next list' b' -- update next list

newList :: Text -> Lists -> Lists
newList s ts = ts |> empty s

delete :: Int -> Lists -> Lists
delete = deleteAt

exists :: Int -> Lists -> Bool
exists i ts = case ts !? i of
    Just _ -> True
    Nothing -> False

shiftBy :: Int -> Int -> Lists -> Maybe Lists
shiftBy = S.shiftBy

search :: Text -> Lists -> Lists
search s ls = searchFor s <$> ls

appendToLast' :: Task -> Lists -> Maybe Lists
appendToLast' t ls = do
    let i = length ls - 1
    l <- ls !? i
    let l' = append t l
    return $ updateLists i ls l'

appendToLast :: Task -> Lists -> Lists
appendToLast t ls = fromMaybe ls $ appendToLast' t ls
