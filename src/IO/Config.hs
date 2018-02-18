{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Config where

import System.Directory
import Data.Ini.Config
import Data.FileEmbed (embedFile)
import qualified Data.ByteString as B (writeFile)

import UI.Theme
import Brick.Themes (themeToAttrMap, loadCustomizations)
import Brick (AttrMap)
import Data.Text (Text, strip, dropAround)
import qualified Data.Text.IO as T

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

defaultConfig :: Config
defaultConfig = Config {
        general = GeneralConfig {
            filename = "taskell.md"
        },
        layout = LayoutConfig {
            columnWidth = 24,
            columnPadding = 3
        },
        markdown = MarkdownConfig {
            titleOutput = "##",
            taskOutput = "-",
            subtaskOutput = "    *"
        }
    }

parseString :: Text -> Text
parseString = dropAround (== '"') . strip

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
writeTheme path = B.writeFile path $(embedFile "templates/theme.ini")

createTheme :: IO ()
createTheme = do
    path <- getThemePath
    exists <- doesFileExist path
    if exists then return () else writeTheme path

writeConfig :: FilePath -> IO ()
writeConfig path = B.writeFile path $(embedFile "templates/config.ini")

createConfig :: IO ()
createConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists then return () else writeConfig path

configParser :: IniParser Config
configParser = do
    generalCf <- section "general" $ do
        filenameCf <- fieldOf "filename" string
        return GeneralConfig { filename = filenameCf }
    layoutCf <- section "layout" $ do
        columnWidthCf <- fieldOf "column_width" number
        columnPaddingCf <- fieldOf "column_padding" number
        return LayoutConfig { columnWidth = columnWidthCf, columnPadding = columnPaddingCf }
    markdownCf <- section "markdown" $ do
        titleOutputCf <- parseString <$> fieldOf "title" string
        taskOutputCf <- parseString <$> fieldOf "task" string
        subtaskOutputCf <- parseString <$> fieldOf "subtask" string
        return MarkdownConfig {
            titleOutput = titleOutputCf,
            taskOutput = taskOutputCf,
            subtaskOutput = subtaskOutputCf
        }
    return Config { general = generalCf, layout = layoutCf, markdown = markdownCf }

getConfig :: IO Config
getConfig = do
    content <- getConfigPath >>= T.readFile
    let config = parseIniFile content configParser

    return $ case config of
        Right c -> c
        Left _ -> defaultConfig

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    path <- getThemePath
    customizedTheme <- loadCustomizations path defaultTheme
    return . themeToAttrMap $ case customizedTheme of
        Left _ -> defaultTheme
        Right theme -> theme
