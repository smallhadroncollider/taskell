{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date.RelativeParser
    ( parseDate
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Data.Time.Calendar  (fromGregorianValid)
import Data.Time.Clock     (addUTCTime)
import Data.Time.LocalTime (LocalTime (LocalTime), midnight)
import Data.Time.Zones     (TZ, localTimeToUTCTZ)

import Utility.Parser (lexeme)

-- relative date parsing
minute :: Int
minute = 60

hour :: Int
hour = minute * 60

day :: Int
day = hour * 24

week :: Int
week = day * 7

periodP :: Char -> Parser Int
periodP c = lexeme decimal <* char c

wP :: Parser Int
wP = (* week) <$> periodP 'w'

dP :: Parser Int
dP = (* day) <$> periodP 'd'

hP :: Parser Int
hP = (* hour) <$> periodP 'h'

mP :: Parser Int
mP = (* minute) <$> periodP 'm'

sP :: Parser Int
sP = periodP 's'

relativeP :: UTCTime -> Parser (Maybe UTCTime)
relativeP now = do
    period <- fromIntegral . sum <$> many1 (sP <|> mP <|> hP <|> dP <|> wP)
    pure $ Just (addUTCTime period now)

-- date parsing
formattedP :: TZ -> Parser (Maybe UTCTime)
formattedP tz = do
    year <- decimal <* char '-'
    month <- decimal <* char '-'
    date <- decimal
    pure $ localTimeToUTCTZ tz . flip LocalTime midnight <$> fromGregorianValid year month date

dateP :: TZ -> UTCTime -> Parser (Maybe UTCTime)
dateP tz now = lexeme (formattedP tz <|> relativeP now)

parseDate :: TZ -> UTCTime -> Text -> Either Text UTCTime
parseDate tz now text =
    case parseOnly (dateP tz now) text of
        Right (Just time) -> Right time
        _                 -> Left "Could not parse date."
