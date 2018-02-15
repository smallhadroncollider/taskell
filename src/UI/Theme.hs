module UI.Theme where

import Brick.Themes (Theme, newTheme)
import Brick (AttrName, attrName)
import Brick.Util (fg)
import Graphics.Vty (defAttr, green, blue, magenta)

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
    newTheme defAttr [
        (titleAttr, fg green),
        (titleCurrentAttr, fg blue),
        (taskCurrentAttr, fg magenta)
    ]
