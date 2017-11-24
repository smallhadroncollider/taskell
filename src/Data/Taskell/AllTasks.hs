module Data.Taskell.AllTasks where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, fromList, (!?), (|>), index, deleteAt)
import qualified Data.Taskell.Seq as S
import Data.Taskell.Tasks (Tasks(..), empty, extract, append)

type AllTasks = Seq Tasks 

initial :: AllTasks
initial = fromList [empty "To Do", empty "Done"]

update :: Int -> AllTasks -> Tasks -> AllTasks
update = S.update 

count :: Int -> AllTasks -> Int
count i ts = case ts !? i of
    Just (Tasks _ ts) -> length ts
    Nothing -> 0

get :: AllTasks -> Int -> Tasks
get = index -- not safe

changeList' :: (Int, Int) -> AllTasks -> Int -> Maybe AllTasks
changeList' (list, index) ts dir = do
    let next = list + dir
    a <- ts !? list -- get current list
    b <- ts !? next -- get next list
    (a', task) <- extract index a -- extract selected task
    let b' = append task b -- add selected task to next list
    let all = update list ts a' -- update extracted list
    return $ update next all b' -- update next list

changeList :: (Int, Int) -> AllTasks -> Int -> AllTasks
changeList cur ts dir = fromMaybe ts (changeList' cur ts dir)

newList :: String -> AllTasks -> AllTasks
newList s ts = ts |> empty s 

delete :: Int -> AllTasks -> AllTasks
delete = deleteAt

exists :: Int -> AllTasks -> Bool
exists i ts = case ts !? i of
    Just _ -> True
    Nothing -> False
