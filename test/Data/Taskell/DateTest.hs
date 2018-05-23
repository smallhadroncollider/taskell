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

testDate :: Day
testDate = fromGregorian 2018 05 18

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
      , testGroup "dayToText" [
           testCase "same year" (
                assertEqual
                    "Date in 18-May format"
                    (Just "18-May")
                    (dayToText <$> fromGregorianValid 2018 08 18 <*> fromGregorianValid 2018 05 18)
           )
         , testCase "different year" (
                assertEqual
                    "Date in 18-May 2019 format"
                    (Just "18-May 2019")
                    (dayToText <$> fromGregorianValid 2018 08 18 <*> fromGregorianValid 2019 05 18)
           )
         , testCase "different year" (
                assertEqual
                    "Date in 18-May 2017 format"
                    (Just "18-May 2017")
                    (dayToText <$> fromGregorianValid 2018 08 18 <*> fromGregorianValid 2017 05 18)
           )
        ]
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
                    Plenty
                    (deadline testDate (fromGregorian 2018 05 28))
            )
          , testCase "ThisWeek" (
                assertEqual
                    "This week"
                    ThisWeek
                    (deadline testDate (fromGregorian 2018 05 24))
            )
          , testCase "Tomorrow" (
                assertEqual
                    "Tomorrow"
                    Tomorrow
                    (deadline testDate (fromGregorian 2018 05 19))
            )
          , testCase "Today" (
                assertEqual
                    "Today"
                    Today
                    (deadline testDate testDate)
            )
          , testCase "Passed" (
                assertEqual
                    "Passed"
                    Passed
                    (deadline testDate (fromGregorian 2018 05 17))
            )
        ]
    ]
