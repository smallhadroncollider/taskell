{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude

import App          (go)
import Events.State (create)
import IO.Config    (setup)
import IO.Taskell   (Next (..), load)

main :: IO ()
main = do
    config <- setup
    next <- runReaderT load config
    case next of
        Exit            -> pure ()
        Output text     -> putStrLn text
        Load path lists -> go config $ create path lists
