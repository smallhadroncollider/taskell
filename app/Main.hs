{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ClassyPrelude

import Events.State (create)
import IO.Taskell (Next(..), load)
import IO.Config (setup)
import App (go)

main :: IO ()
main = do
    config <- setup
    next <- runReaderT load config

    case next of
        Exit -> return ()
        Output text -> putStrLn text
        Load path lists -> go config $ create path lists
