module IO.Config.Layout where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty, parseText)

data Config = Config {
        columnWidth :: Int
      , columnPadding :: Int
      , descriptionIndicator :: Text
    }

defaultConfig :: Config
defaultConfig = Config {
    columnWidth = 30
  , columnPadding = 3
  , descriptionIndicator = "≡"
}

parser :: IniParser Config
parser = fromMaybe defaultConfig <$>
    sectionMb "layout" (do
        columnWidthCf <- fromMaybe (columnWidth defaultConfig) <$> fieldMbOf "column_width" number
        columnPaddingCf <- fromMaybe (columnPadding defaultConfig) <$> fieldMbOf "column_padding" number
        descriptionIndicatorCf <- fromMaybe (descriptionIndicator defaultConfig) . (noEmpty . parseText =<<) <$> fieldMb "description_indicator"
        return Config {
            columnWidth = columnWidthCf
          , columnPadding = columnPaddingCf
          , descriptionIndicator = descriptionIndicatorCf
        }
    )

