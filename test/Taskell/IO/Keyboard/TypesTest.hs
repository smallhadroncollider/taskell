module Taskell.IO.Keyboard.TypesTest
    ( test_types
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Taskell.Events.Actions.Types as A (ActionType (..))
import           Taskell.IO.Keyboard          (addMissing, badMapping, defaultBindings)
import           Taskell.IO.Keyboard.Types    (Binding (..), Bindings)

notFull :: Bindings
notFull = [(BChar 'œ', A.Quit), (BChar 'U', A.Undo)]

notFullResult :: Bindings
notFullResult = notFull <> (drop 2 defaultBindings)

bad :: Bindings
bad = [(BChar 'q', A.Quit), (BChar 'u', A.Nothing), (BChar '>', A.Nothing), (BChar '<', A.ListLeft)]

-- tests
test_types :: TestTree
test_types =
    testGroup
        "Events.Actions.Types"
        [ testCase
              "not missing"
              (assertEqual "Finds no missing items" defaultBindings (addMissing defaultBindings))
        , testCase
              "not missing"
              (assertEqual "Finds missing items" notFullResult (addMissing notFull))
        , testCase
              "bad mapping"
              (assertEqual "Finds bad mapping" (Left "invalid mapping") (badMapping bad))
        ]
