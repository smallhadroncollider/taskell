{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Taskell.Date (
    Deadline(..),
    stringToDay,
    currentDay,
    deadline
) where

import ClassyPrelude

import Data.Time (Day)
import Data.Time.Format (parseTimeM)
import Data.Time.Calendar (diffDays)

data Deadline = Passed | Today | Tomorrow | ThisWeek | Plenty deriving (Show, Eq)

stringToTime :: Text -> Maybe UTCTime
stringToTime = parseTimeM False defaultTimeLocale "%Y-%m-%d" . unpack

stringToDay :: Text -> Maybe Day
stringToDay = (utctDay <$>) . stringToTime

currentDay :: IO Day
currentDay = utctDay <$> getCurrentTime

daysUntil :: Maybe Day -> Maybe Day -> Maybe Integer
daysUntil = liftA2 diffDays

-- work out the deadline
deadline :: Maybe Day -> Maybe Day -> Maybe Deadline
deadline today date = do
    days <- daysUntil date today
    let d | days < 0 = Passed
          | days == 0 = Today
          | days == 1 = Tomorrow
          | days < 7 = ThisWeek
          | otherwise = Plenty
    return d
