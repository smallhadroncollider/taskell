{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Config where

import ClassyPrelude

import qualified Data.Text.IO as T (readFile)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, doesFileExist, doesDirectoryExist)
import System.Environment (lookupEnv)

import Brick (AttrMap)
import Brick.Themes (themeToAttrMap, loadCustomizations)
import Data.FileEmbed (embedFile)
import Data.Ini.Config

import UI.Theme (defaultTheme)

import qualified IO.Config.General as General
import qualified IO.Config.Layout as Layout
import qualified IO.Config.Markdown as Markdown
import qualified IO.Config.Trello as Trello
import qualified IO.Config.GitHub as GitHub

data Config = Config {
        general :: General.Config
      , layout :: Layout.Config
      , markdown :: Markdown.Config
      , trello :: Trello.Config
      , github :: GitHub.Config
    }

defaultConfig :: Config
defaultConfig = Config {
    general = General.defaultConfig
  , layout = Layout.defaultConfig
  , markdown = Markdown.defaultConfig
  , trello = Trello.defaultConfig
  , github = GitHub.defaultConfig
}

directoryName :: FilePath
directoryName = "taskell"

legacyConfigPath :: IO FilePath
legacyConfigPath = (</> "." ++ directoryName) <$> getHomeDirectory

xdgDefaultConfig :: IO FilePath
xdgDefaultConfig = (</> ".config" </> directoryName) <$> getHomeDirectory

xdgConfigPath :: IO FilePath
xdgConfigPath = fromMaybe <$> xdgDefaultConfig <*> lookupEnv "XDG_CONFIG_HOME"

getDir :: IO FilePath
getDir = legacyConfigPath >>= doesDirectoryExist >>= bool xdgConfigPath legacyConfigPath

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

configParser :: IniParser Config
configParser = do

    generalCf <- General.parser
    layoutCf <- Layout.parser
    markdownCf <- Markdown.parser
    trelloCf <- Trello.parser
    githubCf <- GitHub.parser

    return Config {
        general = generalCf
      , layout = layoutCf
      , markdown = markdownCf
      , trello = trelloCf
      , github = githubCf
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

    case customizedTheme of
        Right theme -> return $ themeToAttrMap theme
        Left s -> do
            putStrLn (pack $ "theme.ini: " ++ s)
            return $ themeToAttrMap defaultTheme
