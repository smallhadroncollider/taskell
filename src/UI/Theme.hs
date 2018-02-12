module UI.Theme where

import Brick.Themes (Theme, newTheme, themeToAttrMap, loadCustomizations)
import Brick (AttrName, AttrMap, attrName)
import Brick.Util (fg, on)
import Graphics.Vty (white, brightBlack, green, blue, magenta)
import Persistence.Config (getThemePath)

-- attrs
titleAttr :: AttrName
titleAttr = attrName "title"

titleCurrentAttr :: AttrName
titleCurrentAttr = attrName "titleCurrent"

taskCurrentAttr :: AttrName
taskCurrentAttr = attrName "taskCurrent"

taskAttr :: AttrName
taskAttr = attrName "task"

-- default theme
defaultTheme :: Theme
defaultTheme =
    newTheme (white `on` brightBlack) [
        (attrName "title", fg green),
        (attrName "titleCurrent", fg blue),
        (attrName "taskCurrent", fg magenta)
    ]

-- generate theme
generateAttrMap :: IO AttrMap
generateAttrMap = do
    path <- getThemePath
    customizedTheme <- loadCustomizations path defaultTheme
    return . themeToAttrMap $ case customizedTheme of
        Left _ -> defaultTheme
        Right theme -> theme
