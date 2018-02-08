module UI.Attr where

import Flow.State (State)
import Brick (AttrName, AttrMap, attrMap, attrName)
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

-- map
attrMap' :: State -> AttrMap
attrMap' = const $ attrMap defAttr [
        (titleAttr, fg green),
        (titleCurrentAttr, fg blue),
        (taskCurrentAttr, fg magenta)
    ]
