{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard.TypesTest
    ( test_types
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Events.Actions.Types as A (ActionType (..))
import           IO.Keyboard          (addMissing, badMapping, defaultBindings)
import           IO.Keyboard.Types    (Binding (..), Bindings)

notFull :: Bindings
notFull = [(BChar 'œ', A.Quit), (BChar 'U', A.Undo)]

notFullResult :: Bindings
notFullResult =
    [ (BChar 'œ', A.Quit)
    , (BChar 'U', A.Undo)
    , (BChar 'r', A.Redo)
    , (BChar '/', A.Search)
    , (BChar '!', A.Due)
    , (BChar '?', A.Help)
    , (BChar 'k', A.Previous)
    , (BChar 'j', A.Next)
    , (BChar 'h', A.Left)
    , (BChar 'l', A.Right)
    , (BChar 'g', A.Bottom)
    , (BChar 'a', A.New)
    , (BChar 'O', A.NewAbove)
    , (BChar 'o', A.NewBelow)
    , (BChar '+', A.Duplicate)
    , (BChar 'e', A.Edit)
    , (BChar 'A', A.Edit)
    , (BChar 'i', A.Edit)
    , (BChar 'C', A.Clear)
    , (BChar 'D', A.Delete)
    , (BKey "Enter", A.Detail)
    , (BChar '@', A.DueDate)
    , (BKey "Backspace", A.ClearDate)
    , (BChar 'K', A.MoveUp)
    , (BChar 'J', A.MoveDown)
    , (BChar 'H', A.MoveLeft)
    , (BChar 'L', A.MoveRight)
    , (BKey "Space", A.Complete)
    , (BChar 'm', A.MoveMenu)
    , (BChar 'N', A.ListNew)
    , (BChar 'E', A.ListEdit)
    , (BChar 'X', A.ListDelete)
    , (BChar '>', A.ListRight)
    , (BChar '<', A.ListLeft)
    ]

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
