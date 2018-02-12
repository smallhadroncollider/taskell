module Persistence.Config where

import System.Directory

getDir :: IO FilePath
getDir = (++ "/.taskell") <$> getHomeDirectory

getThemePath :: IO FilePath
getThemePath = do
    dir <- getDir
    return $ dir ++ "/theme.ini"

setup :: IO ()
setup = do
    dir <- getDir
    createDirectoryIfMissing True dir
    createTheme

writeTheme :: FilePath -> IO ()
writeTheme path = writeFile path $ unlines [
        "[default]",
        "default.bg = brightBlack",
        "default.fg = white",
        "",
        "[other]",
        "title.fg = green",
        "titleCurrent.fg = blue",
        "taskCurrent.fg = magenta"
    ]

createTheme :: IO ()
createTheme = do
    path <- getThemePath
    exists <- doesFileExist path
    if exists then return () else writeTheme path
