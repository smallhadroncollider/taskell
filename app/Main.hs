{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ClassyPrelude

import Events.State (create)
import IO.Taskell (exists, readData)
import IO.Config (setup)
import App (go)

main :: IO ()
main = do
    config <- setup
    (exists, path) <- exists config

    when exists $ do
        content <- readData config path

        case content of
            Right lists -> go config $ create path lists
            Left err -> putStrLn . pack $ path ++ ": " ++ err
