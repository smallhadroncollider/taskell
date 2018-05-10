{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Config where

import ClassyPrelude

import Data.Text as T (dropAround, strip)
import qualified Data.Text.IO as T (readFile)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist)

import Brick (AttrMap)
import Brick.Themes (themeToAttrMap, loadCustomizations)
import Data.FileEmbed (embedFile)
import Data.Ini.Config
import IO.Trello (TrelloToken)

import UI.Theme

data GeneralConfig = GeneralConfig {
        filename :: FilePath
    }

data LayoutConfig = LayoutConfig {
        columnWidth :: Int,
        columnPadding :: Int
    }

data MarkdownConfig = MarkdownConfig {
        titleOutput :: Text,
        taskOutput :: Text,
        summaryOutput :: Text,
        subtaskOutput :: Text
    }

data TrelloConfig = TrelloConfig {
        token :: Maybe TrelloToken
    }

data Config = Config {
        general :: GeneralConfig,
        layout :: LayoutConfig,
        markdown :: MarkdownConfig,
        trello :: TrelloConfig
    }

defaultGeneralConfig :: GeneralConfig
defaultGeneralConfig = GeneralConfig {
    filename = "taskell.md"
}

defaultLayoutConfig :: LayoutConfig
defaultLayoutConfig = LayoutConfig {
    columnWidth = 30,
    columnPadding = 3
}

defaultMarkdownConfig :: MarkdownConfig
defaultMarkdownConfig = MarkdownConfig {
    titleOutput = "##",
    taskOutput = "-",
    summaryOutput = "    >",
    subtaskOutput = "    *"
}

defaultTrelloConfig :: TrelloConfig
defaultTrelloConfig = TrelloConfig {
    token = Nothing
}

defaultConfig :: Config
defaultConfig = Config {
    general = defaultGeneralConfig,
    layout = defaultLayoutConfig,
    markdown = defaultMarkdownConfig,
    trello = defaultTrelloConfig
}

getDir :: IO FilePath
getDir = (++ "/.taskell") <$> getHomeDirectory

getThemePath :: IO FilePath
getThemePath = (++ "/theme.ini") <$> getDir

getConfigPath :: IO FilePath
getConfigPath = (++ "/config.ini") <$> getDir

setup :: IO Config
setup = do
    getDir >>= createDirectoryIfMissing True
    createConfig
    createTheme
    getConfig

create :: IO FilePath -> (FilePath -> IO ()) -> IO ()
create getPath write = do
    path <- getPath
    exists <- doesFileExist path
    unless exists $ write path

writeTheme :: FilePath -> IO ()
writeTheme path = writeFile path $(embedFile "templates/theme.ini")

createTheme :: IO ()
createTheme = create getThemePath writeTheme

writeConfig :: FilePath -> IO ()
writeConfig path = writeFile path $(embedFile "templates/config.ini")

createConfig :: IO ()
createConfig = create getConfigPath writeConfig

noEmpty :: Text -> Maybe Text
noEmpty "" = Nothing
noEmpty txt = Just txt

parseText :: Text -> Text
parseText = dropAround (== '"') . strip

configParser :: IniParser Config
configParser = do
    generalCf <- fromMaybe defaultGeneralConfig <$>
        sectionMb "general" (do
            filenameCf <- maybe (filename defaultGeneralConfig) unpack . (noEmpty =<<) <$> fieldMb "filename"
            return GeneralConfig { filename = filenameCf }
        )
    layoutCf <- fromMaybe defaultLayoutConfig <$>
        sectionMb "layout" (do
            columnWidthCf <- fromMaybe (columnWidth defaultLayoutConfig) <$> fieldMbOf "column_width" number
            columnPaddingCf <- fromMaybe (columnPadding defaultLayoutConfig) <$> fieldMbOf "column_padding" number
            return LayoutConfig { columnWidth = columnWidthCf, columnPadding = columnPaddingCf }
        )
    markdownCf <-fromMaybe defaultMarkdownConfig <$>
        sectionMb "markdown" (do
            titleOutputCf <- fromMaybe (titleOutput defaultMarkdownConfig) .  (noEmpty . parseText =<<) <$> fieldMb "title"
            taskOutputCf <- fromMaybe (taskOutput defaultMarkdownConfig) .  (noEmpty . parseText =<<) <$> fieldMb "task"
            summaryOutputCf <- fromMaybe (summaryOutput defaultMarkdownConfig) .  (noEmpty . parseText =<<) <$> fieldMb "summary"
            subtaskOutputCf <- fromMaybe (subtaskOutput defaultMarkdownConfig) .  (noEmpty . parseText =<<) <$> fieldMb "subtask"
            return MarkdownConfig {
                titleOutput = titleOutputCf,
                taskOutput = taskOutputCf,
                summaryOutput = summaryOutputCf,
                subtaskOutput = subtaskOutputCf
            }
        )
    trelloCf <- fromMaybe defaultTrelloConfig <$>
        sectionMb "trello" (do
            tokenCf <- fieldMb "token"
            return TrelloConfig { token = tokenCf }
        )
    return Config {
        general = generalCf,
        layout = layoutCf,
        markdown = markdownCf,
        trello = trelloCf
    }

getConfig :: IO Config
getConfig = do
    content <- getConfigPath >>= T.readFile
    let config = parseIniFile content configParser

    case config of
        Right c -> return c
        Left s -> putStrLn (pack $ "config.ini: " ++ s) >> return defaultConfig

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    path <- getThemePath
    customizedTheme <- loadCustomizations path defaultTheme
    return . themeToAttrMap $ case customizedTheme of
        Left _ -> defaultTheme
        Right theme -> theme
