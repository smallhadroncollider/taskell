module Taskell.UI.Draw.Modal
    ( renderModal
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Taskell.Events.State.Types      (height, mode)
import Taskell.Events.State.Types.Mode (ModalType (..), Mode (..))
import Taskell.UI.Draw.Field           (textField)
import Taskell.UI.Draw.Modal.Detail    (detail)
import Taskell.UI.Draw.Modal.Due       (due)
import Taskell.UI.Draw.Modal.Help      (help)
import Taskell.UI.Draw.Modal.MoveTo    (moveTo)
import Taskell.UI.Draw.Types           (DSWidget, DrawState (dsState), TWidget)
import Taskell.UI.Theme                (titleAttr)
import Taskell.UI.Types                (ResourceName (..))

surround :: (Text, TWidget) -> DSWidget
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

renderModal :: DSWidget
renderModal = do
    md <- (^. mode) <$> asks dsState
    case md of
        Modal (Help s)               -> surround =<< help s
        Modal Detail {}              -> surround =<< detail
        Modal MoveTo                 -> surround =<< moveTo
        Modal (Due tasks selected)   -> surround =<< due tasks selected
        _                            -> pure emptyWidget
