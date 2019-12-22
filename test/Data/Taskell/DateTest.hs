{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.DateTest
    ( test_date
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Taskell.Date
import Data.Time
import Data.Time.Zones     (utcTZ)
import Data.Time.Zones.All (TZLabel (America__New_York), tzByLabel)

testDate :: UTCTime
testDate = UTCTime (fromGregorian 2018 5 18) (secondsToDiffTime 0)

-- tests
test_date :: TestTree
test_date =
    testGroup
        "Data.Taskell.Date"
        [ testGroup
              "timeToDisplay"
              [ testCase
                    "timeToDisplay time"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-18 00:00"
                         (timeToDisplay utcTZ (DueTime testDate)))
              , testCase
                    "timeToDisplay time - non-utc"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-17 20:00"
                         (timeToDisplay (tzByLabel America__New_York) (DueTime testDate)))
              , testCase
                    "timeToDisplay date"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-18"
                         (timeToDisplay utcTZ (DueDate (fromGregorian 2018 05 18))))
              ]
        , testGroup
              "timeToOutput"
              [ testCase
                    "timeToOutput time"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-18 00:00 UTC"
                         (timeToOutput utcTZ (DueTime testDate)))
              , testCase
                    "timeToOutput time - non-utc"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-17 20:00 EDT"
                         (timeToOutput (tzByLabel America__New_York) (DueTime testDate)))
              , testCase
                    "timeToOutput date"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-18"
                         (timeToOutput utcTZ (DueDate (fromGregorian 2018 05 18))))
              , testCase
                    "timeToOutput date - non-utc"
                    (assertEqual
                         "Date in yyyy-mm-dd format"
                         "2018-05-18"
                         (timeToOutput
                              (tzByLabel America__New_York)
                              (DueDate (fromGregorian 2018 05 18))))
              ]
        , testGroup
              "dayToText"
              [ testCase
                    "same year"
                    (assertEqual
                         "Date in 18-May format"
                         "18-May"
                         (timeToText utcTZ testDate (DueTime testDate)))
              , testCase
                    "different year"
                    (assertEqual
                         "Date in 18-May 2019 format"
                         ("18-May 2019")
                         (timeToText utcTZ testDate (DueDate (fromGregorian 2019 5 18))))
              , testCase
                    "different year"
                    (assertEqual
                         "Date in 18-May 2017 format"
                         "18-May 2017"
                         (timeToText utcTZ testDate (DueDate (fromGregorian 2017 5 18))))
              ]
        , testCase
              "textToDay"
              (assertEqual
                   "A valid Day"
                   (DueDate <$> (fromGregorianValid 2018 05 18))
                   (textToTime "2018-05-18"))
        , testGroup
              "deadline"
              [ testCase
                    "Plenty"
                    (assertEqual
                         "Plenty of time"
                         Plenty
                         (deadline testDate (DueDate (fromGregorian 2018 05 28))))
              , testCase
                    "ThisWeek"
                    (assertEqual
                         "This week"
                         ThisWeek
                         (deadline testDate (DueDate (fromGregorian 2018 05 24))))
              , testCase
                    "Tomorrow"
                    (assertEqual
                         "Tomorrow"
                         Tomorrow
                         (deadline testDate (DueDate (fromGregorian 2018 05 19))))
              , testCase "Today" (assertEqual "Today" Today (deadline testDate (DueTime testDate)))
              , testCase
                    "Passed"
                    (assertEqual
                         "Passed"
                         Passed
                         (deadline testDate (DueDate (fromGregorian 2018 05 17))))
              ]
        , testCase
              "isoToTime"
              (assertEqual
                   "Gives back time"
                   (Just (DueTime (UTCTime (fromGregorian 2020 8 11) 82800)))
                   (isoToTime "2020-08-11T23:00:00.000Z"))
        ]
