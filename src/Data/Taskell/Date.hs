{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date
    ( Day
    , Deadline(..)
    , DeadlineFn
    , dayToText
    , dayToOutput
    , textToDay
    , utcToLocalDay
    , deadline
    ) where

import ClassyPrelude

import Data.Time           (Day)
import Data.Time.Calendar  (diffDays, toGregorian)
import Data.Time.Clock     (secondsToDiffTime)
import Data.Time.Format    (formatTime, parseTimeM)
import Data.Time.LocalTime (TimeZone, localDay, utcToZonedTime, zonedTimeToLocalTime)

data Deadline
    = Passed
    | Today
    | Tomorrow
    | ThisWeek
    | Plenty
    deriving (Show, Eq)

type DeadlineFn = Day -> Deadline

dayToText :: Day -> Day -> Text
dayToText today day = pack $ formatTime defaultTimeLocale format (UTCTime day (secondsToDiffTime 0))
  where
    (currentYear, _, _) = toGregorian today
    (dateYear, _, _) = toGregorian day
    format =
        if currentYear == dateYear
            then "%d-%b"
            else "%d-%b %Y"

dayToOutput :: Day -> Text
dayToOutput day = pack $ formatTime defaultTimeLocale "%Y-%m-%d" (UTCTime day (secondsToDiffTime 0))

utcToLocalDay :: TimeZone -> UTCTime -> Day
utcToLocalDay tz = localDay . zonedTimeToLocalTime . utcToZonedTime tz

textToTime :: Text -> Maybe UTCTime
textToTime = parseTimeM False defaultTimeLocale "%Y-%m-%d" . unpack

textToDay :: Text -> Maybe Day
textToDay = (utctDay <$>) . textToTime

-- work out the deadline
deadline :: Day -> Day -> Deadline
deadline today date
    | days < 0 = Passed
    | days == 0 = Today
    | days == 1 = Tomorrow
    | days < 7 = ThisWeek
    | otherwise = Plenty
  where
    days = diffDays date today
