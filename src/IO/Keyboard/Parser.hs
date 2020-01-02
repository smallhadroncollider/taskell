{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module IO.Keyboard.Parser where

import ClassyPrelude

import Data.Attoparsec.Text

import Utility.Parser

import Events.Actions.Types (ActionType, read)
import IO.Keyboard.Types

-- utility functions
commentP :: Parser ()
commentP = lexeme $ skipMany ((char '#' <|> char ';') *> manyTill anyChar endOfLine)

stripComments :: Parser a -> Parser a
stripComments p = lexeme $ commentP *> p <* commentP

-- ini parser
keyP :: Parser Binding
keyP = lexeme $ BKey <$> (char '<' *> word <* char '>')

charP :: Parser Binding
charP = lexeme $ BChar <$> anyChar

bindingP :: Parser [Binding]
bindingP = lexeme $ (keyP <|> charP) `sepBy` char ','

lineP :: Parser [(Binding, ActionType)]
lineP =
    stripComments $ do
        name <- read <$> word
        _ <- lexeme $ char '='
        binds <- bindingP
        pure $ (, name) <$> binds

bindingsP :: Parser Bindings
bindingsP = stripComments $ concat <$> many' lineP

-- run parser
bindings :: Text -> Either Text Bindings
bindings ini = first (const "Could not parse keyboard bindings.") (parseOnly bindingsP ini)
