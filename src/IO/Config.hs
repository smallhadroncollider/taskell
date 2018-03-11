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
        subtaskOutput :: Text
    }

data Config = Config {
        general :: GeneralConfig,
        layout :: LayoutConfig,
        markdown :: MarkdownConfig
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
    subtaskOutput = "    *"
}

defaultConfig :: Config
defaultConfig = Config {
    general = defaultGeneralConfig,
    layout = defaultLayoutConfig,
    markdown = defaultMarkdownConfig
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

writeTheme :: FilePath -> IO ()
writeTheme path = writeFile path $(embedFile "templates/theme.ini")

createTheme :: IO ()
createTheme = do
    path <- getThemePath
    exists <- doesFileExist path
    if exists then return () else writeTheme path

writeConfig :: FilePath -> IO ()
writeConfig path = writeFile path $(embedFile "templates/config.ini")

createConfig :: IO ()
createConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists then return () else writeConfig path

noEmpty :: Maybe Text -> Maybe Text
noEmpty (Just txt) = if null txt then Nothing else Just txt
noEmpty Nothing = Nothing

noEmptyString :: Maybe String -> Maybe String
noEmptyString (Just txt) = if null txt then Nothing else Just txt
noEmptyString Nothing = Nothing

parseString :: Maybe Text -> Maybe Text
parseString (Just s) = Just . dropAround (== '"') $ strip s
parseString Nothing = Nothing

configParser :: IniParser Config
configParser = do
    generalCf <- sectionMb "general" $ do
        filenameCf <- fromMaybe (filename defaultGeneralConfig) . noEmptyString <$> fieldMbOf "filename" string
        return GeneralConfig { filename = filenameCf }
    layoutCf <- sectionMb "layout" $ do
        columnWidthCf <- fromMaybe (columnWidth defaultLayoutConfig) <$> fieldMbOf "column_width" number
        columnPaddingCf <- fromMaybe (columnPadding defaultLayoutConfig) <$> fieldMbOf "column_padding" number
        return LayoutConfig { columnWidth = columnWidthCf, columnPadding = columnPaddingCf }
    markdownCf <- sectionMb "markdown" $ do
        titleOutputCf <- fromMaybe (titleOutput defaultMarkdownConfig) . noEmpty . parseString <$> fieldMb "title"
        taskOutputCf <- fromMaybe (taskOutput defaultMarkdownConfig) . noEmpty . parseString <$> fieldMb "task"
        subtaskOutputCf <- fromMaybe (subtaskOutput defaultMarkdownConfig) . noEmpty . parseString <$> fieldMb "subtask"
        return MarkdownConfig {
            titleOutput = titleOutputCf,
            taskOutput = taskOutputCf,
            subtaskOutput = subtaskOutputCf
        }
    return Config {
        general = fromMaybe defaultGeneralConfig generalCf,
        layout = fromMaybe defaultLayoutConfig layoutCf,
        markdown = fromMaybe defaultMarkdownConfig markdownCf
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
