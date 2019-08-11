{-# LANGUAGE NoImplicitPrelude #-}

module UI.Modal
    ( showModal
    ) where

import ClassyPrelude

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Events.State.Types.Mode (ModalType (..), Mode (..))
import UI.Draw.Types           (DrawState (dsHeight, dsMode), ReaderDrawState)
import UI.Field                (textField)
import UI.Modal.Detail         (detail)
import UI.Modal.Due            (due)
import UI.Modal.Help           (help)
import UI.Modal.MoveTo         (moveTo)
import UI.Theme                (titleAttr)
import UI.Types                (ResourceName (..))

surround :: (Text, Widget ResourceName) -> ReaderDrawState (Widget ResourceName)
surround (title, widget) = do
    ht <- asks dsHeight
    let t = padBottom (Pad 1) . withAttr titleAttr $ textField title
    pure .
        padTopBottom 1 .
        centerLayer .
        border .
        padTopBottom 1 .
        padLeftRight 4 . vLimit (ht - 9) . hLimit 50 . (t <=>) . viewport RNModal Vertical $
        widget

showModal :: ReaderDrawState (Widget ResourceName)
showModal = do
    md <- asks dsMode
    case md of
        Modal Help      -> surround =<< help
        Modal Detail {} -> surround =<< detail
        Modal MoveTo    -> surround =<< moveTo
        Modal Due       -> surround =<< due
        _               -> pure emptyWidget
