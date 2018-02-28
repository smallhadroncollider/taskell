module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode)
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border

import UI.Modal.Help (help)
import UI.Modal.SubTasks (st)
import UI.Modal.MoveTo (moveTo)
import Data.Text as T (Text)

import UI.Types (ResourceName(..))
import UI.Theme (titleAttr)

import UI.Field (textField)

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

getModal :: State -> ModalType -> Widget ResourceName
getModal s t = case t of
    Help -> surround help
    SubTasks _ _ -> surround $ st s
    MoveTo -> surround $ moveTo s

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal s t : view
    _ -> view
