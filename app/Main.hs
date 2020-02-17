module Main where

import ClassyPrelude

import System.Exit (die)

import Data.Time.Zones (loadLocalTZ)

import Taskell              (go)
import Taskell.Events.State (create)
import Taskell.IO           (IOInfo (IOInfo), Next (..), load)
import Taskell.IO.Config    (setup)

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
