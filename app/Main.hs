module Main where

import Control.Monad (when)
import Events.State (create)
import IO.Taskell (exists, readFile)
import IO.Config (Config, setup)

import App (go)

-- read file then render
start :: Config -> FilePath -> IO ()
start config path = do
    state <- create path <$> IO.Taskell.readFile config path
    go config state

-- if taskell.md exists/created then start
main :: IO ()
main = do
    config <- setup
    (ex, path) <- exists config
    when ex $ start config path
