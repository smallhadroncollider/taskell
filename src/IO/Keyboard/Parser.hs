{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module IO.Keyboard.Parser where

import ClassyPrelude

import Data.Attoparsec.Text

import IO.Keyboard.Types

-- utility functions
lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

commentP :: Parser ()
commentP = lexeme $ skipMany (char '#' *> manyTill anyChar endOfLine)

stripComments :: Parser a -> Parser a
stripComments p = lexeme $ commentP *> p <* commentP

word :: Parser Text
word = lexeme $ pack <$> many1 letter

-- ini parser
keyP :: Parser Binding
keyP = lexeme $ BKey <$> (char '<' *> word <* char '>')

charP :: Parser Binding
charP = lexeme $ BChar <$> anyChar

bindingP :: Parser [Binding]
bindingP = lexeme $ (keyP <|> charP) `sepBy` char ','

line :: Parser [(Binding, Text)]
line =
    stripComments $ do
        name <- word
        _ <- lexeme $ char '='
        binds <- bindingP
        pure $ (, name) <$> binds

bindingsP :: Parser Bindings
bindingsP = stripComments $ concat <$> many' line

-- run parser
bindings :: Text -> Either Text Bindings
bindings ini = first (const "Could not parse keyboard bindings.") (parseOnly bindingsP ini)
