{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date.RelativeParser
    ( parseDate
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Utility.Parser (lexeme)

import Data.Time.Calendar (fromGregorianValid)
import Data.Time.Clock    (addUTCTime, secondsToDiffTime)

-- date utility functions
toTime :: Integer -> (Integer, Int, Int) -> Maybe UTCTime
toTime seconds (y, m, d) = flip UTCTime (secondsToDiffTime seconds) <$> fromGregorianValid y m d

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
formattedP :: Parser (Maybe UTCTime)
formattedP = toTime 0 <$> ((,,) <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal)

dateP :: UTCTime -> Parser (Maybe UTCTime)
dateP now = lexeme (formattedP <|> relativeP now)

parseDate :: UTCTime -> Text -> Either Text UTCTime
parseDate now text =
    case parseOnly (dateP now) text of
        Right (Just time) -> Right time
        _                 -> Left "Could not parse date."
