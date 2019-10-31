{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude

import System.Exit (die)

import App          (go)
import Events.State (create)
import IO.Config    (setup)
import IO.Taskell   (Next (..), load)

main :: IO ()
main = do
    config <- setup
    next <- runReaderT load config
    time <- getCurrentTime
    case next of
        Exit            -> pure ()
        Output text     -> putStrLn text
        Error text      -> die $ unpack text
        Load path lists -> go config $ create time path lists
