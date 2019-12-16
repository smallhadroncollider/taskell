{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude

import System.Exit (die)

import Data.Time.Zones (loadLocalTZ)

import App          (go)
import Events.State (create)
import IO.Config    (setup)
import IO.Taskell   (IOInfo (IOInfo), Next (..), load)

main :: IO ()
main = do
    config <- setup
    timezone <- loadLocalTZ
    next <- runReaderT load (IOInfo timezone config)
    time <- getCurrentTime
    case next of
        Exit            -> pure ()
        Output text     -> putStrLn text
        Error text      -> die $ unpack text
        Load path lists -> go config $ create timezone time path lists
