{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date
    ( Day
    , Deadline(..)
    , toTime
    , timeToText
    , timeToOutput
    , textToTime
    , deadline
    ) where

import ClassyPrelude

import Control.Lens       ((^.))
import Control.Lens.Tuple (_1)

import Data.Time.Zones (TZ, utcToLocalTimeTZ)

import Data.Time.Calendar (diffDays, fromGregorianValid, toGregorian)
import Data.Time.Clock    (secondsToDiffTime)
import Data.Time.Format   (formatTime, parseTimeM)

data Deadline
    = Passed
    | Today
    | Tomorrow
    | ThisWeek
    | Plenty
    deriving (Show, Eq)

toTime :: Integer -> (Integer, Int, Int) -> Maybe UTCTime
toTime seconds (y, m, d) = flip UTCTime (secondsToDiffTime seconds) <$> fromGregorianValid y m d

getYear :: UTCTime -> Integer
getYear = (^. _1) . toGregorian . utctDay

timeToText :: TZ -> UTCTime -> UTCTime -> Text
timeToText tz now date = pack $ formatTime defaultTimeLocale format time
  where
    time = utcToLocalTimeTZ tz date
    format =
        if getYear now == getYear date
            then "%d-%b"
            else "%d-%b %Y"

timeToOutput :: TZ -> UTCTime -> Text
timeToOutput tz time = pack $ formatTime defaultTimeLocale "%Y-%m-%d" (utcToLocalTimeTZ tz time)

textToTime :: Text -> Maybe UTCTime
textToTime = parseTimeM False defaultTimeLocale "%Y-%m-%d" . unpack

-- work out the deadline
deadline :: UTCTime -> UTCTime -> Deadline
deadline now date
    | days < 0 = Passed
    | days == 0 = Today
    | days == 1 = Tomorrow
    | days < 7 = ThisWeek
    | otherwise = Plenty
  where
    days = diffDays (utctDay date) (utctDay now)
