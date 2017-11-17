module Actions (event) where

import State
import Graphics.Vty.Input.Events
import Keyboard

type Action = ([KeyCheck], StateUpdate)

-- list of keyboard actions
fns :: [Action]
fns = [
        (([isChar 'q']), quit),
        ([isUp, isChar 'k'], previous),
        ([isDown, isChar 'j'], next),
        ([isChar ' '], setCompleted)
    ]

-- check if any of the KeyChecks for an Action pass
pass :: Event -> Action -> Bool
pass e (kcs, _) = any (\x -> x e) kcs

-- filter a list of pass checks
-- not the most efficient, always goes through full list
passes :: (Action -> Bool) -> [Action]
passes = flip filter fns

-- return the actions function, if there is one
getFn :: [Action] -> StateUpdate
getFn [] = id
getFn [x] = snd x

-- pass gets curried with Event
-- which is filtered using passes
-- which is then passed to getFn
-- which is applied to State
event :: Event -> State -> State
event = getFn . passes . pass
