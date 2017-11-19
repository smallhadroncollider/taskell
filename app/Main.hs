module Main where

import Draw (render)
import State (initial, tasks)
import TaskellJSON (exists, readJSON)

start :: IO ()
start = do
    ts <- readJSON
    render initial { tasks = ts }

main :: IO ()
main = do
    go <- exists 
    if go then start else return ()
