{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Main
    ( renderMain
    ) where

import ClassyPrelude

import Brick
import Control.Lens  ((^.))
import Data.Sequence (mapWithIndex)

import Events.State.Types     (lists)
import UI.Draw.Main.List      (renderList)
import UI.Draw.Main.Search    (renderSearch)
import UI.Draw.Main.StatusBar (renderStatusBar)
import UI.Draw.Types          (DSWidget, DrawState (..))
import UI.Types               (ResourceName (..))

renderMain :: DSWidget
renderMain = do
    ls <- (^. lists) <$> asks dsState
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    let mainWidget = viewport RNLists Horizontal . padTopBottom 1 $ hBox listWidgets
    statusBar <- renderStatusBar
    renderSearch (mainWidget <=> statusBar)
