{-# LANGUAGE NoImplicitPrelude #-}
module UI.Modal (
    showModal
) where

import ClassyPrelude

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border

import Data.Taskell.Date (DeadlineFn)
import Events.State (State, Mode(..), ModalType(..), mode)
import UI.Field (textField)
import UI.Modal.Help (help)
import UI.Modal.MoveTo (moveTo)
import UI.Modal.Detail (detail)
import UI.Theme (titleAttr)
import UI.Types (ResourceName(..))

surround :: (Text, Widget ResourceName) -> Widget ResourceName
surround (title, widget) =
      padTopBottom 1
    . centerLayer
    . border
    . padTopBottom 1
    . padLeftRight 4
    . hLimit 50
    . (t <=>)
    . viewport RNModal Vertical
    $ widget

    where t = padBottom (Pad 1) . withAttr titleAttr $ textField title

showModal :: State -> DeadlineFn -> [Widget ResourceName] -> [Widget ResourceName]
showModal s deadlineFn view = case mode s of
    Modal Help -> surround help : view
    Modal Detail {} -> surround (detail s deadlineFn) : view
    Modal MoveTo -> surround (moveTo s) : view
    _ -> view
