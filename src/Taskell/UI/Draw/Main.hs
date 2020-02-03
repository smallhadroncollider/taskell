{-# LANGUAGE NoImplicitPrelude #-}

module Taskell.UI.Draw.Main
    ( renderMain
    ) where

import ClassyPrelude

import Brick
import Control.Lens  ((^.))
import Data.Sequence (mapWithIndex)

import Taskell.Events.State.Types     (lists)
import Taskell.IO.Config.Layout       (padding, statusBar)
import Taskell.UI.Draw.Main.List      (renderList)
import Taskell.UI.Draw.Main.Search    (renderSearch)
import Taskell.UI.Draw.Main.StatusBar (renderStatusBar)
import Taskell.UI.Draw.Types          (DSWidget, DrawState (..))
import Taskell.UI.Types               (ResourceName (..))

renderMain :: DSWidget
renderMain = do
    ls <- (^. lists) <$> asks dsState
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    pad <- padding <$> asks dsLayout
    let mainWidget = viewport RNLists Horizontal . padTopBottom pad $ hBox listWidgets
    sb <- bool emptyWidget <$> renderStatusBar <*> (statusBar <$> asks dsLayout)
    renderSearch (mainWidget <=> sb)
