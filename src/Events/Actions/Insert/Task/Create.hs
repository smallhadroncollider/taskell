module Events.Actions.Insert.Task.Create (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . (below =<<) . (removeBlank =<<) . store
event _ = return
