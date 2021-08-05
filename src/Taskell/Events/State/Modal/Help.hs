module Taskell.Events.State.Modal.Help where

import ClassyPrelude
import Control.Lens ((&), (.~), (^.))
import Taskell.Events.State.Types (Stateful, mode)
import Taskell.Events.State.Types.Mode
  ( DetailItem (..),
    DetailMode (..),
    HelpScrollPosition (..),
    ModalType (Detail, Help),
    Mode (Modal),
  )

moveHelpMenuUp :: Stateful
moveHelpMenuUp = pure . (mode .~ Modal (Help Bottom))

moveHelpMenuDown :: Stateful
moveHelpMenuDown = pure . (mode .~ Modal (Help Top))
