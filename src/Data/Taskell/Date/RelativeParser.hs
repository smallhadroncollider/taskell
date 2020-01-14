{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Taskell.Date.RelativeParser
    ( parseRelative
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Data.Time.Clock (addUTCTime)

import Data.Taskell.Date.Types (Due (DueTime))
import Utility.Parser          (lexeme)

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
relativeP now =
    lexeme $ do
        period <- fromIntegral . sum <$> many1 (sP <|> mP <|> hP <|> dP <|> wP)
        pure $ Just (addUTCTime period now)

parseRelative :: UTCTime -> Text -> Either Text Due
parseRelative now text =
    case parseOnly (relativeP now) text of
        Right (Just time) -> Right (DueTime time)
        _                 -> Left "Could not parse date."
