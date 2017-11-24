module Main where

import Render (render)
import Flow.State (create, setTasks)
import Persistence.Taskell (exists, readJSON)
import System.Console.Terminal.Size (Window(..), size)

import Data.Bool

quitIfFalseOtherwise :: IO () -> Bool -> IO ()
quitIfFalseOtherwise = bool $ return ()

getSize :: IO (Int, Int)
getSize = do
    s <- size
    case s of
        Just (Window h w) -> return (w, h)
        Nothing -> return (80, 30)

-- read JSON then render
start :: IO ()
start = do
    json <- readJSON
    size <- getSize
    render $ create size json

-- if taskell.json exists/created then start
main :: IO ()
main = exists >>= quitIfFalseOtherwise start 
