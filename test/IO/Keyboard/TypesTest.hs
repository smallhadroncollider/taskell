{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Keyboard.TypesTest
    ( test_types
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Events.Actions.Types (ActionType (..))
import IO.Keyboard.Types    (Binding (..), Bindings, badMapping, missing)

full :: Bindings
full =
    [ (BChar 'q', AQuit)
    , (BChar 'u', AUndo)
    , (BChar '/', ASearch)
    , (BChar '?', AHelp)
    , (BChar 'k', APrevious)
    , (BChar 'j', ANext)
    , (BChar 'h', ALeft)
    , (BChar 'l', ARight)
    , (BChar 'g', ABottom)
    , (BChar 'a', ANew)
    , (BChar 'O', ANewAbove)
    , (BChar 'o', ANewBelow)
    , (BChar 'i', AEdit)
    , (BChar 'C', AClear)
    , (BChar 'D', ADelete)
    , (BKey "Enter", ADetail)
    , (BChar '@', ADueDate)
    , (BChar 'K', AMoveUp)
    , (BChar 'J', AMoveDown)
    , (BChar 'H', AMoveLeft)
    , (BChar 'L', AMoveRight)
    , (BKey "Space", AMoveRight)
    , (BChar 'm', AMoveMenu)
    , (BChar 'N', AListNew)
    , (BChar 'E', AListEdit)
    , (BChar 'X', AListDelete)
    , (BChar '>', AListRight)
    , (BChar '<', AListLeft)
    ]

notFull :: Bindings
notFull =
    [ (BChar 'q', AQuit)
    , (BChar 'u', AUndo)
    , (BChar '/', ASearch)
    , (BChar '?', AHelp)
    , (BChar 'k', APrevious)
    , (BChar 'j', ANext)
    , (BChar 'h', ALeft)
    , (BChar 'l', ARight)
    , (BChar 'g', ABottom)
    , (BChar 'i', AEdit)
    , (BChar 'C', AClear)
    , (BChar 'D', ADelete)
    , (BKey "Enter", ADetail)
    , (BChar '@', ADueDate)
    , (BChar 'K', AMoveUp)
    , (BChar 'J', AMoveDown)
    , (BChar 'H', AMoveLeft)
    , (BChar 'L', AMoveRight)
    , (BKey "Space", AMoveRight)
    , (BChar 'm', AMoveMenu)
    , (BChar 'N', AListNew)
    , (BChar 'E', AListEdit)
    , (BChar 'X', AListDelete)
    , (BChar '>', AListRight)
    , (BChar '<', AListLeft)
    ]

bad :: Bindings
bad = [(BChar 'q', AQuit), (BChar 'u', ANothing), (BChar '>', ANothing), (BChar '<', AListLeft)]

-- tests
test_types :: TestTree
test_types =
    testGroup
        "Events.Actions.Types"
        [ testCase "not missing" (assertEqual "Finds no missing items" Nothing (missing full))
        , testCase
              "not missing"
              (assertEqual
                   "Finds missing items"
                   (Just [ANew, ANewAbove, ANewBelow])
                   (missing notFull))
        , testCase
              "bad mapping"
              (assertEqual "Finds bad mapping" (Just [BChar 'u', BChar '>']) (badMapping bad))
        ]
