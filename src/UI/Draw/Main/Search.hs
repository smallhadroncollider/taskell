{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Main.Search
    ( renderSearch
    ) where

import Brick
import ClassyPrelude
import Control.Lens  ((^.))

import Events.State.Types      (mode, searchTerm)
import Events.State.Types.Mode (Mode (..))
import IO.Config.Layout        (columnPadding)
import UI.Draw.Field           (field)
import UI.Draw.Types           (DrawState (..), ReaderDrawState)
import UI.Theme
import UI.Types                (ResourceName (..))

renderSearch :: Widget ResourceName -> ReaderDrawState (Widget ResourceName)
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
