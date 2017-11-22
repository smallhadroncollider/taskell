module Main where

import Render (render)
import Flow.State (create, setTasks)
import Persistence.Taskell (exists, readJSON)

import Data.Bool

quitIfFalseOtherwise :: IO () -> Bool -> IO ()
quitIfFalseOtherwise = bool $ return ()

-- read JSON then render
start :: IO ()
start = readJSON >>= render . create 

-- if taskell.json exists/created then start
main :: IO ()
main = exists >>= quitIfFalseOtherwise start 
