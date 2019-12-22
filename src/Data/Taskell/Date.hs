{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date
    ( Day
    , Deadline(..)
    , Due(..)
    , timeToText
    , timeToDisplay
    , timeToOutput
    , textToTime
    , isoToTime
    , deadline
    ) where

import ClassyPrelude

import Control.Lens       ((^.))
import Control.Lens.Tuple (_1)

import Data.Time.LocalTime (ZonedTime (ZonedTime))
import Data.Time.Zones     (TZ, timeZoneForUTCTime, utcToLocalTimeTZ)

import Data.Time.Calendar (diffDays, toGregorian)
import Data.Time.Format   (FormatTime, ParseTime, formatTime, iso8601DateFormat, parseTimeM)

data Due
    = DueTime UTCTime
    | DueDate Day
    deriving (Show, Eq, Ord)

data Deadline
    = Passed
    | Today
    | Tomorrow
    | ThisWeek
    | Plenty
    deriving (Show, Eq)

-- formats
dateFormat :: String
dateFormat = "%Y-%m-%d"

timeDisplayFormat :: String
timeDisplayFormat = "%Y-%m-%d %H:%M"

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M %Z"

isoFormat :: String
isoFormat = iso8601DateFormat (Just "%H:%M:%S%Q%Z")

-- utility functions
utcToZonedTime :: TZ -> UTCTime -> ZonedTime
utcToZonedTime tz time = ZonedTime (utcToLocalTimeTZ tz time) (timeZoneForUTCTime tz time)

dayToYear :: Day -> Integer
dayToYear = (^. _1) . toGregorian

getYear :: Due -> Integer
getYear (DueTime t) = dayToYear $ utctDay t
getYear (DueDate d) = dayToYear d

-- output
format :: (FormatTime t) => String -> t -> String
format = formatTime defaultTimeLocale

timeToText :: TZ -> UTCTime -> Due -> Text
timeToText tz now date = pack $ format fmt time
  where
    time =
        utcToLocalTimeTZ tz $
        case date of
            DueTime t -> t
            DueDate d -> UTCTime d 0
    fmt =
        if getYear (DueTime now) == getYear date
            then "%d-%b"
            else "%d-%b %Y"

timeToDisplay :: TZ -> Due -> Text
timeToDisplay _ (DueDate day)   = pack $ format dateFormat day
timeToDisplay tz (DueTime time) = pack $ format timeDisplayFormat (utcToLocalTimeTZ tz time)

timeToOutput :: TZ -> Due -> Text
timeToOutput _ (DueDate day)   = pack $ format dateFormat day
timeToOutput tz (DueTime time) = pack $ format timeFormat (utcToZonedTime tz time)

-- input
parseT :: (Monad m, ParseTime t) => Text -> String -> m t
parseT txt fmt = parseTimeM False defaultTimeLocale fmt (unpack txt)

textToTime :: Text -> Maybe Due
textToTime txt =
    if length txt == 10 -- not a great check, needs to use parser
        then DueDate <$> parseT txt dateFormat
        else DueTime <$> parseT txt timeFormat

isoToTime :: Text -> Maybe Due
isoToTime txt = DueTime <$> parseT txt isoFormat

-- deadlines
deadline :: UTCTime -> Due -> Deadline
deadline now date
    | days < 0 = Passed
    | days == 0 = Today
    | days == 1 = Tomorrow
    | days < 7 = ThisWeek
    | otherwise = Plenty
  where
    days =
        case date of
            DueTime t -> diffDays (utctDay t) (utctDay now)
            DueDate d -> diffDays d (utctDay now)
