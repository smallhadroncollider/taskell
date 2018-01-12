module Main where

import Prelude hiding (readFile)
import Control.Monad (when)
import Flow.State (create)
import Persistence.Taskell (exists, readFile)
import System.Console.Terminal.Size (Window(..), size)

import App (go)

getSize :: IO (Int, Int)
getSize = do
    s <- size
    case s of
        Just (Window h w) -> return (w, h)
        Nothing -> return (80, 30)

-- read JSON then render
start :: FilePath -> IO ()
start path = do
    content <- readFile path
    s <- getSize
    go $ create path s content

-- if taskell.json exists/created then start
main :: IO ()
main = do
    (ex, path) <- exists
    when ex $ start path
