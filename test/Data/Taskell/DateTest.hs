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
        testCase "dayToOutput" (
            assertEqual
                "Date in yyyy-mm-dd format"
                (Just "2018-05-18")
                (dayToOutput <$> fromGregorianValid 2018 05 18)
        )
      , testCase "dayToText" (
            assertEqual
                "Date in 18-May format"
                (Just "18-May")
                (dayToText <$> fromGregorianValid 2018 05 18)
        )
      , testCase "textToDay" (
            assertEqual
                "A valid Day"
                (fromGregorianValid 2018 05 18)
                (textToDay "2018-05-18")
        )
      , testGroup "deadline" [
            testCase "Plenty" (
                assertEqual
                    "Plenty of time"
                    (Just Plenty)
                    (deadline (textToDay "2018-05-18") (textToDay "2018-05-28"))
            )
          , testCase "ThisWeek" (
                assertEqual
                    "This week"
                    (Just ThisWeek)
                    (deadline (textToDay "2018-05-18") (textToDay "2018-05-24"))
            )
          , testCase "Tomorrow" (
                assertEqual
                    "Tomorrow"
                    (Just Tomorrow)
                    (deadline (textToDay "2018-05-18") (textToDay "2018-05-19"))
            )
          , testCase "Today" (
                assertEqual
                    "Today"
                    (Just Today)
                    (deadline (textToDay "2018-05-18") (textToDay "2018-05-18"))
            )
          , testCase "Passed" (
                assertEqual
                    "Passed"
                    (Just Passed)
                    (deadline (textToDay "2018-05-18") (textToDay "2018-05-17"))
            )
        ]
    ]
