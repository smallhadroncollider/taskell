{-# LANGUAGE NoImplicitPrelude #-}

module UI.Theme
    ( titleAttr
    , titleCurrentAttr
    , taskCurrentAttr
    , taskAttr
    , disabledAttr
    , dlToAttr
    , defaultTheme
    ) where

import Brick        (AttrName, attrName)
import Brick.Themes (Theme, newTheme)
import Brick.Util   (fg)
import Graphics.Vty (blue, defAttr, green, magenta, red, yellow)

import Data.Taskell.Date (Deadline (..))

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

dlDue, dlSoon, dlFar :: AttrName
dlDue = attrName "dlDue"

dlSoon = attrName "dlSoon"

dlFar = attrName "dlFar"

-- convert deadline into attribute
dlToAttr :: Deadline -> AttrName
dlToAttr dl =
    case dl of
        Plenty   -> dlFar
        ThisWeek -> dlSoon
        Tomorrow -> dlSoon
        Today    -> dlDue
        Passed   -> dlDue

-- default theme
defaultTheme :: Theme
defaultTheme =
    newTheme
        defAttr
        [ (titleAttr, fg green)
        , (titleCurrentAttr, fg blue)
        , (taskCurrentAttr, fg magenta)
        , (disabledAttr, fg yellow)
        , (dlDue, fg red)
        , (dlSoon, fg yellow)
        , (dlFar, fg green)
        ]
