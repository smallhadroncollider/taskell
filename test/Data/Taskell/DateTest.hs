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
import Data.Time.Zones   (loadLocalTZ)

testDate :: UTCTime
testDate = UTCTime (fromGregorian 2018 5 18) (secondsToDiffTime 0)

-- tests
test_date :: IO TestTree
test_date = do
    tz <- loadLocalTZ
    pure $
        testGroup
            "Data.Taskell.Date"
            [ testCase
                  "dayToOutput"
                  (assertEqual
                       "Date in yyyy-mm-dd format"
                       (Just "2018-05-18")
                       (timeToOutput tz <$> toTime 0 (2018, 05, 18)))
            , testGroup
                  "dayToText"
                  [ testCase
                        "same year"
                        (assertEqual
                             "Date in 18-May format"
                             (Just "18-May")
                             (timeToText tz <$> toTime 0 (2018, 08, 18) <*> toTime 0 (2018, 05, 18)))
                  , testCase
                        "different year"
                        (assertEqual
                             "Date in 18-May 2019 format"
                             (Just "18-May 2019")
                             (timeToText tz <$> toTime 0 (2018, 08, 18) <*> toTime 0 (2019, 05, 18)))
                  , testCase
                        "different year"
                        (assertEqual
                             "Date in 18-May 2017 format"
                             (Just "18-May 2017")
                             (timeToText tz <$> toTime 0 (2018, 08, 18) <*> toTime 0 (2017, 05, 18)))
                  ]
            , testCase
                  "textToDay"
                  (assertEqual "A valid Day" (toTime 0 (2018, 05, 18)) (textToTime "2018-05-18"))
            , testGroup
                  "deadline"
                  [ testCase
                        "Plenty"
                        (assertEqual
                             "Plenty of time"
                             Plenty
                             (deadline testDate (UTCTime (fromGregorian 2018 05 28) 0)))
                  , testCase
                        "ThisWeek"
                        (assertEqual
                             "This week"
                             ThisWeek
                             (deadline testDate (UTCTime (fromGregorian 2018 05 24) 0)))
                  , testCase
                        "Tomorrow"
                        (assertEqual
                             "Tomorrow"
                             Tomorrow
                             (deadline testDate (UTCTime (fromGregorian 2018 05 19) 0)))
                  , testCase "Today" (assertEqual "Today" Today (deadline testDate testDate))
                  , testCase
                        "Passed"
                        (assertEqual
                             "Passed"
                             Passed
                             (deadline testDate (UTCTime (fromGregorian 2018 05 17) 0)))
                  ]
            ]
