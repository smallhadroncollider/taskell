module Main where

import Control.Monad (when)
import Flow.State (create)
import Persistence.Taskell (exists, readFile)
import Persistence.Config (Config, setup)
import System.Console.Terminal.Size (Window(..), size)

import App (go)

getSize :: IO (Int, Int)
getSize = do
    s <- size
    case s of
        Just (Window h w) -> return (w, h)
        Nothing -> return (80, 30)

-- read file then render
start :: Config -> FilePath -> IO ()
start config path = do
    content <- Persistence.Taskell.readFile path
    s <- getSize
    go config $ create path s content

-- if taskell.md exists/created then start
main :: IO ()
main = do
    config <- setup
    (ex, path) <- exists config
    when ex $ start config path
