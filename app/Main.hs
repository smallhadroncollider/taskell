module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.Bool
import System.Directory

import Draw (render)
import Task (jsonToTasks)
import State

path :: FilePath
path = "taskell.json"

-- creates taskell file if passed False 
createFile :: Bool -> IO ()
createFile = bool (writeFile path "[]") (return ())

main :: IO ()
main = do
    doesFileExist path >>= createFile

    x <- readFile path 
    let ts = jsonToTasks x

    render (State {
        tasks = ts,
        current = 0,
        running = True,
        showCompleted = True
    }) 
