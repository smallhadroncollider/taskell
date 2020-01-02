module IO.Config.Layout where

import ClassyPrelude

import Data.Ini.Config

import IO.Config.Parser (noEmpty, parseText)

data Config = Config
    { padding              :: Int
    , columnWidth          :: Int
    , columnPadding        :: Int
    , descriptionIndicator :: Text
    , statusBar            :: Bool
    }

defaultConfig :: Config
defaultConfig =
    Config
    {padding = 1, columnWidth = 30, columnPadding = 3, descriptionIndicator = "≡", statusBar = True}

paddingP :: SectionParser Int
paddingP = fromMaybe (padding defaultConfig) <$> fieldMbOf "padding" number

columnWidthP :: SectionParser Int
columnWidthP = fromMaybe (columnWidth defaultConfig) <$> fieldMbOf "column_width" number

columnPaddingP :: SectionParser Int
columnPaddingP = fromMaybe (columnPadding defaultConfig) <$> fieldMbOf "column_padding" number

descriptionIndicatorP :: SectionParser Text
descriptionIndicatorP =
    fromMaybe (descriptionIndicator defaultConfig) . (noEmpty . parseText =<<) <$>
    fieldMb "description_indicator"

statusBarP :: SectionParser Bool
statusBarP = fieldFlagDef "statusbar" (statusBar defaultConfig)

parser :: IniParser Config
parser =
    fromMaybe defaultConfig <$>
    sectionMb
        "layout"
        (Config <$> paddingP <*> columnWidthP <*> columnPaddingP <*> descriptionIndicatorP <*>
         statusBarP)
