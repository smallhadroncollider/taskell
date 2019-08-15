{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UI.Draw.Main.StatusBar
    ( renderStatusBar
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import Data.Taskell.Lists (count)
import Events.State.Types (current, lists, mode, path, searchTerm)

import Events.State.Types.Mode (ModalType (..), Mode (..))
import IO.Config.Layout        (columnPadding)
import UI.Draw.Field           (Field)
import UI.Draw.Types           (DrawState (..), ReaderDrawState)
import UI.Theme
import UI.Types                (ResourceName (..))

getPosition :: ReaderDrawState Text
getPosition = do
    (col, pos) <- (^. current) <$> asks dsState
    len <- count col . (^. lists) <$> asks dsState
    let posNorm =
            if len > 0
                then pos + 1
                else 0
    pure $ tshow posNorm <> "/" <> tshow len

modeToText :: Maybe Field -> Mode -> ReaderDrawState Text
modeToText fld md = do
    debug <- asks dsDebug
    pure $
        if debug
            then tshow md
            else case md of
                     Normal ->
                         case fld of
                             Nothing -> "NORMAL"
                             Just _  -> "NORMAL + SEARCH"
                     Insert {} -> "INSERT"
                     Modal Help -> "HELP"
                     Modal MoveTo -> "MOVE"
                     Modal Detail {} -> "DETAIL"
                     Search {} -> "SEARCH"
                     _ -> ""

getMode :: ReaderDrawState Text
getMode = do
    state <- asks dsState
    modeToText (state ^. searchTerm) (state ^. mode)

renderStatusBar :: ReaderDrawState (Widget ResourceName)
renderStatusBar = do
    topPath <- pack . (^. path) <$> asks dsState
    colPad <- columnPadding <$> asks dsLayout
    posTxt <- getPosition
    modeTxt <- getMode
    let titl = padLeftRight colPad $ txt topPath
    let pos = padRight (Pad colPad) $ txt posTxt
    let md = txt modeTxt
    let bar = padRight Max (titl <+> md) <+> pos
    pure . padTop (Pad 1) $ withAttr statusBarAttr bar
