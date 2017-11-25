module Main where

import Render (render)
import Control.Monad (when)
import Flow.State (create)
import Persistence.Taskell (exists, readJSON, writeJSON)
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
start :: FilePath -> IO ()
start path = do
    json <- readJSON path
    size <- getSize
    render path $ create size json

-- if taskell.json exists/created then start
main :: IO ()
main = do
    (ex, path) <- exists
    when ex $ start path
