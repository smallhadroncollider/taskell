{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date.RelativeParser
    ( parseRelative
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Data.Time.Clock (addUTCTime)

import Data.Taskell.Date.Types (Due (DueDate, DueTime))
import Utility.Parser          (lexeme, only)

-- utility functions
addP :: (Integral a) => Parser a -> UTCTime -> Parser UTCTime
addP p now = ($ now) . addUTCTime . fromIntegral . sum <$> many1 p

-- relative time parsing
minute :: Int
minute = 60

hour :: Int
hour = minute * 60

day :: Int
day = hour * 24

week :: Int
week = day * 7

timePeriodP :: Char -> Parser Int
timePeriodP c = lexeme decimal <* char c

wP :: Parser Int
wP = (* week) <$> timePeriodP 'w'

dP :: Parser Int
dP = (* day) <$> timePeriodP 'd'

hP :: Parser Int
hP = (* hour) <$> timePeriodP 'h'

mP :: Parser Int
mP = (* minute) <$> timePeriodP 'm'

sP :: Parser Int
sP = timePeriodP 's'

timeP :: UTCTime -> Parser (Maybe Due)
timeP now = only . lexeme $ Just . DueTime <$> addP (sP <|> mP <|> hP <|> dP <|> wP) now

-- relative date parsing
dateP :: UTCTime -> Parser (Maybe Due)
dateP now = only . lexeme $ Just . DueDate . utctDay <$> addP (dP <|> wP) now

-- relative parser
relativeP :: UTCTime -> Parser (Maybe Due)
relativeP now = dateP now <|> timeP now

parseRelative :: UTCTime -> Text -> Either Text Due
parseRelative now text =
    case parseOnly (relativeP now) text of
        Right (Just due) -> Right due
        _                -> Left "Could not parse date."
