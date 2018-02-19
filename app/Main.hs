module Main where

import Control.Monad (when)
import Events.State (create)
import qualified IO.Taskell as T (exists, readFile)
import IO.Config (setup)

import App (go)

main :: IO ()
main = do
    config <- setup
    (exists, path) <- T.exists config

    when exists $ do
        content <- T.readFile config path

        case content of
            Right lists -> go config $ create path lists
            Left err -> putStrLn $ path ++ ": " ++ err
