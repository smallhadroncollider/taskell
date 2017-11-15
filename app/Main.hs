module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)

import Draw (render)
import Task

path :: FilePath
path = "taskell.json"

tasksExist :: Maybe Tasks -> Bool
tasksExist (Just ts) = True
tasksExist Nothing = False

deMaybe :: Maybe Tasks -> Tasks
deMaybe (Just ts) = ts
deMaybe Nothing = []

main :: IO ()
main = do
    x <- readFile path 
    let ts = jsonToTasks x
    if tasksExist ts
        then render $ deMaybe ts
        else putStrLn "No tasks found"
