{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taskell.UI.Draw.Main.Search
    ( renderSearch
    ) where

import Brick
import ClassyPrelude
import Control.Lens  ((^.))

import Taskell.Events.State.Types      (mode, searchTerm)
import Taskell.Events.State.Types.Mode (Mode (..))
import Taskell.IO.Config.Layout        (columnPadding)
import Taskell.UI.Draw.Field           (field)
import Taskell.UI.Draw.Types           (DSWidget, DrawState (..))
import Taskell.UI.Theme
import Taskell.UI.Types                (ResourceName (..))

renderSearch :: Widget ResourceName -> DSWidget
renderSearch mainWidget = do
    m <- (^. mode) <$> asks dsState
    term <- (^. searchTerm) <$> asks dsState
    case term of
        Just searchField -> do
            colPad <- columnPadding . dsLayout <$> ask
            let attr =
                    withAttr $
                    case m of
                        Search -> taskCurrentAttr
                        _      -> taskAttr
            let widget = attr . padLeftRight colPad $ txt "/" <+> field searchField
            pure $ mainWidget <=> widget
        _ -> pure mainWidget
