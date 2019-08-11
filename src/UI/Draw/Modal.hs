{-# LANGUAGE NoImplicitPrelude #-}

module UI.Draw.Modal
    ( renderModal
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Events.State.Types      (height, mode)
import Events.State.Types.Mode (ModalType (..), Mode (..))
import UI.Draw.Field           (textField)
import UI.Draw.Modal.Detail    (detail)
import UI.Draw.Modal.Due       (due)
import UI.Draw.Modal.Help      (help)
import UI.Draw.Modal.MoveTo    (moveTo)
import UI.Draw.Types           (DrawState (dsState), ReaderDrawState)
import UI.Theme                (titleAttr)
import UI.Types                (ResourceName (..))

surround :: (Text, Widget ResourceName) -> ReaderDrawState (Widget ResourceName)
surround (title, widget) = do
    ht <- (^. height) <$> asks dsState
    let t = padBottom (Pad 1) . withAttr titleAttr $ textField title
    pure .
        padTopBottom 1 .
        centerLayer .
        border .
        padTopBottom 1 .
        padLeftRight 4 . vLimit (ht - 9) . hLimit 50 . (t <=>) . viewport RNModal Vertical $
        widget

renderModal :: ReaderDrawState (Widget ResourceName)
renderModal = do
    md <- (^. mode) <$> asks dsState
    case md of
        Modal Help      -> surround =<< help
        Modal Detail {} -> surround =<< detail
        Modal MoveTo    -> surround =<< moveTo
        Modal Due       -> surround =<< due
        _               -> pure emptyWidget
