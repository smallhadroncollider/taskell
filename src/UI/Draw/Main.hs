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
import UI.Draw.Types          (DrawState (..), ReaderDrawState)
import UI.Types               (ResourceName (..))

renderMain :: ReaderDrawState (Widget ResourceName)
renderMain = do
    ls <- (^. lists) <$> asks dsState
    listWidgets <- toList <$> sequence (renderList `mapWithIndex` ls)
    let mainWidget = viewport RNLists Horizontal . padTopBottom 1 $ hBox listWidgets
    statusBar <- renderStatusBar
    renderSearch (mainWidget <=> statusBar)
