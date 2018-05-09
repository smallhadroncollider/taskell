{-# LANGUAGE NoImplicitPrelude #-}
module UI.Theme where

import Brick.Themes (Theme, newTheme)
import Brick (AttrName, attrName)
import Brick.Util (fg)
import Graphics.Vty (defAttr, green, blue, magenta, yellow)

-- attrs
titleAttr :: AttrName
titleAttr = attrName "title"

titleCurrentAttr :: AttrName
titleCurrentAttr = attrName "titleCurrent"

taskCurrentAttr :: AttrName
taskCurrentAttr = attrName "taskCurrent"

taskAttr :: AttrName
taskAttr = attrName "task"

disabledAttr :: AttrName
disabledAttr = attrName "disabled"

-- default theme
defaultTheme :: Theme
defaultTheme =
    newTheme defAttr [
        (titleAttr, fg green),
        (titleCurrentAttr, fg blue),
        (taskCurrentAttr, fg magenta),
        (disabledAttr, fg yellow)
    ]
