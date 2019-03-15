{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.Config where

import ClassyPrelude

import qualified Data.Text.IO       as T (readFile)
import           System.Directory   (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                     getHomeDirectory)
import           System.Environment (lookupEnv)

import Brick           (AttrMap)
import Brick.Themes    (loadCustomizations, themeToAttrMap)
import Data.FileEmbed  (embedFile)
import Data.Ini.Config

import UI.Theme (defaultTheme)

import qualified IO.Config.General  as General
import qualified IO.Config.GitHub   as GitHub
import qualified IO.Config.Layout   as Layout
import qualified IO.Config.Markdown as Markdown
import qualified IO.Config.Trello   as Trello

data Config = Config
    { general  :: General.Config
    , layout   :: Layout.Config
    , markdown :: Markdown.Config
    , trello   :: Trello.Config
    , github   :: GitHub.Config
    }

defaultConfig :: Config
defaultConfig =
    Config
        General.defaultConfig
        Layout.defaultConfig
        Markdown.defaultConfig
        Trello.defaultConfig
        GitHub.defaultConfig

directoryName :: FilePath
directoryName = "taskell"

legacyConfigPath :: IO FilePath
legacyConfigPath = (</> "." <> directoryName) <$> getHomeDirectory

xdgDefaultConfig :: IO FilePath
xdgDefaultConfig = (</> ".config" </> directoryName) <$> getHomeDirectory

xdgConfigPath :: IO FilePath
xdgConfigPath = fromMaybe <$> xdgDefaultConfig <*> lookupEnv "XDG_CONFIG_HOME"

getDir :: IO FilePath
getDir = legacyConfigPath >>= doesDirectoryExist >>= bool xdgConfigPath legacyConfigPath

getThemePath :: IO FilePath
getThemePath = (<> "/theme.ini") <$> getDir

getConfigPath :: IO FilePath
getConfigPath = (<> "/config.ini") <$> getDir

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

configParser :: IniParser Config
configParser =
    Config <$> General.parser <*> Layout.parser <*> Markdown.parser <*> Trello.parser <*>
    GitHub.parser

getConfig :: IO Config
getConfig = do
    content <- getConfigPath >>= T.readFile
    case parseIniFile content configParser of
        Right config -> pure config
        Left s       -> putStrLn (pack $ "config.ini: " <> s) *> pure defaultConfig

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    customizedTheme <- flip loadCustomizations defaultTheme =<< getThemePath
    case customizedTheme of
        Right theme -> pure $ themeToAttrMap theme
        Left s      -> putStrLn (pack $ "theme.ini: " <> s) $> themeToAttrMap defaultTheme
