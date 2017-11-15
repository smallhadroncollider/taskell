module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)

import Draw (render)
import Task (jsonToTasks)

main :: IO ()
main = do
    x <- readFile "taskell.json" 
    render $ jsonToTasks x
