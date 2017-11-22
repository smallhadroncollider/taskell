module Data.Taskell.AllTasks where

import Data.Map.Strict (Map, fromList)
import Data.Taskell.Tasks (Tasks, empty)

type AllTasks = Map String Tasks 

initial :: AllTasks
initial = fromList [("To Do", empty), ("Done", empty)]
