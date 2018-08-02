module IO.Config.Parser where

import ClassyPrelude

import Data.Text as T (dropAround, strip)

noEmpty :: Text -> Maybe Text
noEmpty "" = Nothing
noEmpty txt = Just txt

parseText :: Text -> Text
parseText = dropAround (== '"') . strip
