{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.Data.DateTest
    ( test_date
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time
import Data.Time.Zones     (utcTZ)
import Data.Time.Zones.All (TZLabel (America__New_York), tzByLabel)
import Taskell.Data.Date

testDate :: UTCTime
testDate = UTCTime (fromGregorian 2018 5 18) (secondsToDiffTime 0)

-- sorting test data
sort1 :: Due
sort1 = DueTime (UTCTime (fromGregorian 2017 2 4) 0)

sort2 :: Due
sort2 = DueDate (fromGregorian 2016 12 15)

sort3 :: Due
sort3 = DueTime (UTCTime (fromGregorian 2020 8 9) 44000)

sort4 :: Due
sort4 = DueDate (fromGregorian 2019 8 30)

-- tests
test_date :: TestTree
test_date =
    testGroup
        "Data.Taskell.Date"
        [ testCase
              "Sorting"
              (assertEqual
                   "Sorted in date order"
                   [sort2, sort1, sort4, sort3]
                   (sort [sort1, sort2, sort3, sort4]))
        , testGroup
              "Date Output"
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
                               (timeToOutput (DueTime testDate)))
                    , testCase
                          "timeToOutput date"
                          (assertEqual
                               "Date in yyyy-mm-dd format"
                               "2018-05-18"
                               (timeToOutput (DueDate (fromGregorian 2018 05 18))))
                    ]
              , testGroup
                    "timeToOutputLocal"
                    [ testCase
                          "timeToOutputLocal time"
                          (assertEqual
                               "Date in yyyy-mm-dd format"
                               "2018-05-18 00:00 UTC"
                               (timeToOutputLocal utcTZ (DueTime testDate)))
                    , testCase
                          "timeToOutputLocal time - non-utc"
                          (assertEqual
                               "Date in yyyy-mm-dd format"
                               "2018-05-17 20:00 EDT"
                               (timeToOutputLocal (tzByLabel America__New_York) (DueTime testDate)))
                    , testCase
                          "timeToOutputLocal date"
                          (assertEqual
                               "Date in yyyy-mm-dd format"
                               "2018-05-18"
                               (timeToOutputLocal utcTZ (DueDate (fromGregorian 2018 05 18))))
                    , testCase
                          "timeToOutputLocal date - non-utc"
                          (assertEqual
                               "Date in yyyy-mm-dd format"
                               "2018-05-18"
                               (timeToOutputLocal
                                    (tzByLabel America__New_York)
                                    (DueDate (fromGregorian 2018 05 18))))
                    ]
              , testGroup
                    "timeToText"
                    [ testCase
                          "time"
                          (assertEqual
                               "Date in 18-May format"
                               "00:00 18-May"
                               (timeToText utcTZ testDate (DueTime testDate)))
                    , testCase
                          "time - non-utc"
                          (assertEqual
                               "Date in 17-May format"
                               "20:00 17-May"
                               (timeToText (tzByLabel America__New_York) testDate (DueTime testDate)))
                    , testCase
                          "same year"
                          (assertEqual
                               "Date in 18-May format"
                               "18-May"
                               (timeToText utcTZ testDate (DueDate (fromGregorian 2018 5 18))))
                    , testCase
                          "different year"
                          (assertEqual
                               "Date in 18-May 2019 format"
                               "18-May 2019"
                               (timeToText utcTZ testDate (DueDate (fromGregorian 2019 5 18))))
                    , testCase
                          "different year"
                          (assertEqual
                               "Date in 18-May 2017 format"
                               "18-May 2017"
                               (timeToText utcTZ testDate (DueDate (fromGregorian 2017 5 18))))
                    ]
              ]
        , testGroup
              "Input"
              [ testGroup
                    "textToTime"
                    [ testCase
                          "simple date"
                          (assertEqual
                               "A valid Day"
                               (DueDate <$> fromGregorianValid 2018 05 18)
                               (textToTime "2018-05-18"))
                    , testCase
                          "simple time"
                          (assertEqual
                               "A valid time"
                               (DueTime . flip UTCTime 64800 <$> fromGregorianValid 2018 05 18)
                               (textToTime "2018-05-18 18:00 UTC"))
                    , testCase
                          "time with timezone"
                          (assertEqual
                               "A valid time"
                               (DueTime . flip UTCTime 0 <$> fromGregorianValid 2018 05 18)
                               (textToTime "2018-05-17 20:00 EDT"))
                    ]
              , testGroup
                    "inputToTime"
                    [ testCase
                          "simple date"
                          (assertEqual
                               "A valid Day"
                               (DueDate <$> fromGregorianValid 2018 05 18)
                               (inputToTime utcTZ testDate "2018-05-18"))
                    , testCase
                          "simple time"
                          (assertEqual
                               "A valid time"
                               (DueTime . flip UTCTime 64800 <$> fromGregorianValid 2018 05 18)
                               (inputToTime utcTZ testDate "2018-05-18 18:00"))
                    , testCase
                          "time with timezone"
                          (assertEqual
                               "A valid time"
                               (DueTime . flip UTCTime 79200 <$> fromGregorianValid 2018 05 18)
                               (inputToTime
                                    (tzByLabel America__New_York)
                                    testDate
                                    "2018-05-18 18:00"))
                    , testCase
                          "relative time - seconds"
                          (assertEqual
                               "Adds 7 seconds"
                               (DueTime . flip UTCTime 7 <$> fromGregorianValid 2018 05 18)
                               (inputToTime utcTZ testDate "7s"))
                    , testCase
                          "relative time - days"
                          (assertEqual
                               "Adds 29 days, 12 hours"
                               (DueTime . flip UTCTime 43200 <$> fromGregorianValid 2018 06 16)
                               (inputToTime utcTZ testDate "29 d 12h"))
                    , testCase
                          "relative time - weeks"
                          (assertEqual
                               "Adds four weeks, two days, 12 hours"
                               (DueTime . flip UTCTime 43200 <$> fromGregorianValid 2018 06 17)
                               (inputToTime utcTZ testDate "4w 2d 12h"))
                    ]
              , testCase
                    "isoToTime"
                    (assertEqual
                         "Gives back time"
                         (Just (DueTime (UTCTime (fromGregorian 2020 8 11) 82800)))
                         (isoToTime "2020-08-11T23:00:00.000Z"))
              ]
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
        ]
