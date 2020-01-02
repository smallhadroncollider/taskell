{-# LANGUAGE NoImplicitPrelude #-}

module Utility.Parser where

import ClassyPrelude

import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

line :: Parser Text
line = (takeTill isEndOfLine <* endOfLine) <|> takeText

word :: Parser Text
word = lexeme $ pack <$> many1 letter
