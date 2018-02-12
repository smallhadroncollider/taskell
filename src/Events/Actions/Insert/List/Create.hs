module Events.Actions.Insert.List.Create (event) where

import Graphics.Vty.Input.Events
import Events.State

event :: Event -> Stateful
event (EvKey KEnter _) = (write =<<) . (startCreate =<<) . (newItem =<<) . (store =<<) . createList
event (EvKey KEsc _) = normalMode
event (EvKey KBS _) = createListBS
event (EvKey (KChar char) _) = createListChar char
event _ = return
