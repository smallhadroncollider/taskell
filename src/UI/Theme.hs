{-# LANGUAGE NoImplicitPrelude #-}

module UI.Theme
    ( titleAttr
    , titleCurrentAttr
    , taskCurrentAttr
    , taskAttr
    , disabledAttr
    , dlPomodoroTask
    , dlPomodoroShortBreak
    , dlPomodoroLongBreak
    , dlPomodoroTaskOvertime
    , dlPomodoroShortBreakOvertime
    , dlPomodoroLongBreakOvertime
    , dlToAttr
    , defaultTheme
    ) where

import Brick                   (AttrName, attrName)
import Brick.Themes            (Theme, newTheme)
import Brick.Util              (fg)
import Graphics.Vty            (blue, defAttr, green, magenta, red, yellow)
import Graphics.Vty.Attributes (blink, withStyle, MaybeDefault(KeepCurrent), Attr(Attr))

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

dlPomodoroTask = attrName "dlPomodoroTask"

dlPomodoroShortBreak = attrName "dlPomodoroShortBreak"

dlPomodoroLongBreak = attrName "dlPomodoroLongBreak"

-- Ideally, we'd have a style mod, so that blink can be applied to any AttrName
-- but I don't know how to do that, after looking at Brick.Widget.Core
-- I assume, in order to support Themes, this can't be done
dlPomodoroTaskOvertime = attrName "dlPomodoroTaskOvertime"

dlPomodoroShortBreakOvertime = attrName "dlPomodoroShortBreakOvertime"

dlPomodoroLongBreakOvertime = attrName "dlPomodoroLongBreakOvertime"

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
        , (dlPomodoroTask, fg red)
        , (dlPomodoroShortBreak, fg green)
        , (dlPomodoroLongBreak, fg blue)
        , (dlPomodoroTaskOvertime, withStyle (fg red) blink)
        , (dlPomodoroShortBreakOvertime, withStyle (fg green) blink)
        , (dlPomodoroLongBreakOvertime, withStyle (fg blue) blink)
        ]
    where
        keepEverythingAttr = Attr KeepCurrent KeepCurrent KeepCurrent KeepCurrent
