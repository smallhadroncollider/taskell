{-# LANGUAGE NoImplicitPrelude #-}

module Taskell.Utility.Parser where

import ClassyPrelude

import Data.Attoparsec.Text

only :: Parser a -> Parser a
only p = p <* endOfInput

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

line :: Parser Text
line = (takeTill isEndOfLine <* endOfLine) <|> takeText

word :: Parser Text
word = lexeme $ pack <$> many1 letter
