module Data.Taskell.Lists where

import Data.Sequence (Seq, fromList, (!?), (|>), deleteAt)
import qualified Data.Taskell.Seq as S
import Data.Taskell.List (List(..), empty, extract, append)

type Lists = Seq List 

initial :: Lists
initial = fromList [empty "To Do", empty "Done"]

update :: Int -> Lists -> List -> Lists
update = S.update 

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
    let list' = update list ts a' -- update extracted list
    return $ update next list' b' -- update next list

newList :: String -> Lists -> Lists
newList s ts = ts |> empty s 

delete :: Int -> Lists -> Lists
delete = deleteAt

exists :: Int -> Lists -> Bool
exists i ts = case ts !? i of
    Just _ -> True
    Nothing -> False

shiftBy :: Int -> Int -> Lists -> Maybe Lists
shiftBy = S.shiftBy
