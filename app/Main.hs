module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)

import Draw (render)
import Task (jsonToTasks)
import State

main :: IO ()
main = do
    x <- readFile "taskell.json" 
    let ts = jsonToTasks x

    render (State {
        tasks = ts,
        current = 0,
        running = True,
        showCompleted = True
    }) 
