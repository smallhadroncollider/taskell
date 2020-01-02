{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Main
    ( renderMain
    ) where

import ClassyPrelude

import Brick
import Control.Lens  ((^.))
import Data.Sequence (mapWithIndex)

import Events.State.Types     (lists)
import IO.Config.Layout       (padding, statusBar)
import UI.Draw.Main.List      (renderList)
import UI.Draw.Main.Search    (renderSearch)
import UI.Draw.Main.StatusBar (renderStatusBar)
import UI.Draw.Types          (DSWidget, DrawState (..))
import UI.Types               (ResourceName (..))

renderMain :: DSWidget
renderMain = do
    ls <- (^. lists) <$> asks dsState
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    pad <- padding <$> asks dsLayout
    let mainWidget = viewport RNLists Horizontal . padTopBottom pad $ hBox listWidgets
    showStatusBar <- statusBar <$> asks dsLayout
    sb <- renderStatusBar
    let sb' =
            if showStatusBar
                then sb
                else emptyWidget
    renderSearch (mainWidget <=> sb')
