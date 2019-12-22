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
    , deadline
    ) where

import ClassyPrelude

import Control.Lens       ((^.))
import Control.Lens.Tuple (_1)

import Data.Time.LocalTime (ZonedTime (ZonedTime))
import Data.Time.Zones     (TZ, timeZoneForUTCTime, utcToLocalTimeTZ)

import Data.Time.Calendar (diffDays, toGregorian)
import Data.Time.Format   (formatTime, parseTimeM)

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

dateFormat :: String
dateFormat = "%Y-%m-%d"

timeDisplayFormat :: String
timeDisplayFormat = "%Y-%m-%d %H:%M"

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M %Z"

utcToZonedTime :: TZ -> UTCTime -> ZonedTime
utcToZonedTime tz time = ZonedTime (utcToLocalTimeTZ tz time) (timeZoneForUTCTime tz time)

getYear :: Due -> Integer
getYear (DueTime t) = (^. _1) . toGregorian $ utctDay t
getYear (DueDate d) = (^. _1) $ toGregorian d

timeToText :: TZ -> UTCTime -> Due -> Text
timeToText tz now date = pack $ formatTime defaultTimeLocale format time
  where
    time =
        utcToLocalTimeTZ tz $
        case date of
            DueTime t -> t
            DueDate d -> UTCTime d 0
    format =
        if getYear (DueTime now) == getYear date
            then "%d-%b"
            else "%d-%b %Y"

timeToDisplay :: TZ -> Due -> Text
timeToDisplay _ (DueDate day) = pack $ formatTime defaultTimeLocale dateFormat day
timeToDisplay tz (DueTime time) =
    pack $ formatTime defaultTimeLocale timeDisplayFormat (utcToLocalTimeTZ tz time)

timeToOutput :: TZ -> Due -> Text
timeToOutput _ (DueDate day) = pack $ formatTime defaultTimeLocale dateFormat day
timeToOutput tz (DueTime time) =
    pack $ formatTime defaultTimeLocale timeFormat (utcToZonedTime tz time)

textToTime :: Text -> Maybe Due
textToTime txt =
    if length txt == 10 -- not a great check, needs to use parser
        then DueDate <$> parseTimeM False defaultTimeLocale dateFormat (unpack txt)
        else DueTime <$> parseTimeM False defaultTimeLocale timeFormat (unpack txt)

-- work out the deadline
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
