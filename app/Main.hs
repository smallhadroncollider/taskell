module Main where

import UI.Render (render)
import Flow.State (initial, setTasks)
import Persistence.Taskell (exists, readJSON)

import Data.Bool

quitIfFalseOtherwise :: IO () -> Bool -> IO ()
quitIfFalseOtherwise = bool $ return ()

-- read JSON then render
start :: IO ()
start = readJSON >>= render . setTasks initial

-- if taskell.json exists/created then start
main :: IO ()
main = exists >>= quitIfFalseOtherwise start 
