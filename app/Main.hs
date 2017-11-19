module Main where

import Draw (render)
import State (initial, setTasks)
import TaskellJSON (exists, readJSON)

import Data.Bool

quitIfFalseOtherwise :: IO () -> Bool -> IO ()
quitIfFalseOtherwise = bool $ return ()

-- read JSON then render
start :: IO ()
start = readJSON >>= render . setTasks initial

-- if taskell.json exists/created then start
main :: IO ()
main = exists >>= quitIfFalseOtherwise start 
