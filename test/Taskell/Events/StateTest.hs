{-# LANGUAGE OverloadedLists #-}

module Taskell.Events.StateTest
    ( test_state
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens ((&), (.~), (^.))

import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Zones (utcTZ)

import qualified Data.Sequence                   as S (lookup)
import qualified Taskell.Data.List               as L (append, empty)
import qualified Taskell.Data.Task               as T (new)
import           Taskell.Events.State
import           Taskell.Events.State.Types
import           Taskell.Events.State.Types.Mode
import           Taskell.Types                   (ListIndex (ListIndex), TaskIndex (TaskIndex))

mockTime :: UTCTime
mockTime = UTCTime (ModifiedJulianDay 20) (secondsToDiffTime 0)

testState :: State
testState =
    State
    { _mode = Normal
    , _history = fresh empty
    , _path = "test.md"
    , _io = Nothing
    , _height = 0
    , _searchTerm = Nothing
    , _time = mockTime
    , _timeZone = utcTZ
    }

moveToState :: State
moveToState =
    State
    { _mode = Modal MoveTo
    , _history =
          History
          { _past = empty
          , _present =
                ( (ListIndex 4, TaskIndex 0)
                , [ L.empty "List 1"
                  , L.empty "List 2"
                  , L.empty "List 3"
                  , L.empty "List 4"
                  , L.append (T.new "Test Item") (L.empty "List 5")
                  , L.empty "List 6"
                  , L.empty "List 7"
                  , L.empty "List 8"
                  , L.empty "List 9"
                  ])
          , _future = empty
          }
    , _path = "test.md"
    , _io = Nothing
    , _height = 0
    , _searchTerm = Nothing
    , _time = mockTime
    , _timeZone = utcTZ
    }

-- tests
test_state :: TestTree
test_state =
    testGroup
        "Events.State"
        [ testCase
              "quit"
              (assertEqual
                   "Retuns Just with mode set to Shutdown"
                   (Just (testState & mode .~ Shutdown))
                   (quit testState))
        , testCase
              "continue"
              (assertEqual "Retuns with io set to " (testState & io .~ Nothing) (continue testState))
        -- 1 - a, 2 - b, 3 - c, 4 - d, 5 - e, 6 - f, 7 - g, 8 - h, 9 - i
        , testGroup
              "moveTo"
              [ testCase
                    "first"
                    (assertEqual
                         "Moves to first list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 1")))
                         (S.lookup 0 . (^. lists) =<< moveTo 'a' moveToState))
              , testCase
                    "second"
                    (assertEqual
                         "Moves to second list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 2")))
                         (S.lookup 1 . (^. lists) =<< moveTo 'b' moveToState))
              , testCase
                    "fourth"
                    (assertEqual
                         "Moves to fourth list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 4")))
                         (S.lookup 3 . (^. lists) =<< moveTo 'd' moveToState))
              , testCase
                    "sixth"
                    (assertEqual
                         "Moves to sixth list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 6")))
                         (S.lookup 5 . (^. lists) =<< moveTo 'f' moveToState))
              , testCase
                    "penultimate"
                    (assertEqual
                         "Moves to penultime list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 8")))
                         (S.lookup 7 . (^. lists) =<< moveTo 'h' moveToState))
              , testCase
                    "last"
                    (assertEqual
                         "Moves to last list"
                         (Just (L.append (T.new "Test Item") (L.empty "List 9")))
                         (S.lookup 8 . (^. lists) =<< moveTo 'i' moveToState))
              , testCase
                    "current list"
                    (assertEqual "Doesn't do anything" Nothing (moveTo 'e' moveToState))
              , testCase
                    "invalid option"
                    (assertEqual "Doesn't do anything" Nothing (moveTo 'q' moveToState))
              ]
        ]
