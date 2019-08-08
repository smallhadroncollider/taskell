{-# LANGUAGE NoImplicitPrelude #-}

module UI.Modal
    ( showModal
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Data.Taskell.Date       (Day)
import Events.State.Types      (State, mode)
import Events.State.Types.Mode (ModalType (..), Mode (..))
import IO.Keyboard.Types       (Bindings)
import UI.Field                (textField)
import UI.Modal.Detail         (detail)
import UI.Modal.Due            (due)
import UI.Modal.Help           (help)
import UI.Modal.MoveTo         (moveTo)
import UI.Theme                (titleAttr)
import UI.Types                (ResourceName (..))

surround :: Int -> (Text, Widget ResourceName) -> Widget ResourceName
surround ht (title, widget) =
    padTopBottom 1 .
    centerLayer .
    border .
    padTopBottom 1 .
    padLeftRight 4 . vLimit (ht - 9) . hLimit 50 . (t <=>) . viewport RNModal Vertical $
    widget
  where
    t = padBottom (Pad 1) . withAttr titleAttr $ textField title

showModal :: Int -> Bindings -> State -> Day -> Widget ResourceName
showModal ht bindings state today =
    case state ^. mode of
        Modal Help      -> surround ht (help bindings)
        Modal Detail {} -> surround ht (detail state today)
        Modal MoveTo    -> surround ht (moveTo state)
        Modal Due       -> surround ht (due state today)
        _               -> emptyWidget
