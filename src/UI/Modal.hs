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
import UI.Field                (textField)
import UI.Modal.Detail         (detail)
import UI.Modal.Help           (help)
import UI.Modal.MoveTo         (moveTo)
import UI.Theme                (titleAttr)
import UI.Types                (ResourceName (..))

surround :: (Text, Widget ResourceName) -> Widget ResourceName
surround (title, widget) =
    padTopBottom 1 .
    centerLayer .
    border . padTopBottom 1 . padLeftRight 4 . hLimit 50 . (t <=>) . viewport RNModal Vertical $
    widget
  where
    t = padBottom (Pad 1) . withAttr titleAttr $ textField title

showModal :: State -> Day -> [Widget ResourceName] -> [Widget ResourceName]
showModal state today view =
    case state ^. mode of
        Modal Help      -> surround help : view
        Modal Detail {} -> surround (detail state today) : view
        Modal MoveTo    -> surround (moveTo state) : view
        _               -> view
