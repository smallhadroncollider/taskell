module Data.Taskell.AllTasks where

import Data.Sequence (Seq, fromList)
import Data.Taskell.Tasks (Tasks, empty)

type AllTasks = Seq Tasks 

initial :: AllTasks
initial = fromList [empty "To Do", empty "Done"]
