module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import System.Directory
import System.IO (stdout, hFlush)

import Draw (render)
import Task (jsonToTasks)
import State

path :: FilePath
path = "taskell.json"

-- creates taskell file
createPath :: IO ()
createPath = writeFile path "[]"

-- prompt whether to create taskell.json
prompt :: Bool -> IO Bool
prompt True = return True
prompt False = do
    cwd <- getCurrentDirectory
    putStr ("Create " ++ cwd ++ "/" ++ path ++ "? (y/n): ")
    hFlush stdout -- prevents buffering 
    r <- getLine
    if r == "y" then (createPath >> return True) else return False

start :: IO ()
start = do
    x <- readFile path 
    render initial { tasks = jsonToTasks x }

main :: IO ()
main = do
    go <- doesFileExist path >>= prompt
    if go then start else return ()
