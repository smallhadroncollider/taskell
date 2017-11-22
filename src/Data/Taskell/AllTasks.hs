module Data.Taskell.AllTasks where

import Data.Sequence (Seq, fromList, (!?), index)
import qualified Data.Taskell.Seq as S
import Data.Taskell.Tasks (Tasks(..), empty)

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
