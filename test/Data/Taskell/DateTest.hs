{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.DateTest (
    test_date
) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import Data.Time
import Data.Taskell.Date

-- tests
test_date :: TestTree
test_date =
    testGroup "Data.Taskell.Date" [
        testCase "stringToDay" (
            assertEqual
                "Plenty of time"
                (fromGregorianValid 2018 05 18)
                (stringToDay "2018-05-18")
        )
      , testGroup "deadline" [
            testCase "Plenty" (
                assertEqual
                    "Plenty of time"
                    (Just Plenty)
                    (deadline (stringToDay "2018-05-18") (stringToDay "2018-05-28"))
            )
          , testCase "ThisWeek" (
                assertEqual
                    "This week"
                    (Just ThisWeek)
                    (deadline (stringToDay "2018-05-18") (stringToDay "2018-05-24"))
            )
          , testCase "Tomorrow" (
                assertEqual
                    "Tomorrow"
                    (Just Tomorrow)
                    (deadline (stringToDay "2018-05-18") (stringToDay "2018-05-19"))
            )
          , testCase "Today" (
                assertEqual
                    "Today"
                    (Just Today)
                    (deadline (stringToDay "2018-05-18") (stringToDay "2018-05-18"))
            )
          , testCase "Passed" (
                assertEqual
                    "Passed"
                    (Just Passed)
                    (deadline (stringToDay "2018-05-18") (stringToDay "2018-05-17"))
            )
        ]
    ]
