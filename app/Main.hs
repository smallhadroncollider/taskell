module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import System.Directory

import Draw (render)
import Task (jsonToTasks)
import State
import CLI (promptYN)

path :: FilePath
path = "taskell.json"

-- creates taskell file
createPath :: IO ()
createPath = writeFile path "[]"

-- prompt whether to create taskell.json
promptCreate :: Bool -> IO Bool
promptCreate True = return True
promptCreate False = do
    cwd <- getCurrentDirectory
    r <- promptYN $ "Create " ++ cwd ++ "/" ++ path ++ "?"
    if r then (createPath >> return True) else return False

start :: IO ()
start = do
    x <- readFile path 
    render initial { tasks = jsonToTasks x }

main :: IO ()
main = do
    go <- doesFileExist path >>= promptCreate
    if go then start else return ()
