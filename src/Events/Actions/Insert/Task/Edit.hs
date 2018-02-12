module Events.Actions.Insert.Task.Edit (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . (removeBlank =<<) . normalMode
event _ = return
