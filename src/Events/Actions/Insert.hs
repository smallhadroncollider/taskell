{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Insert (event) where

import ClassyPrelude

import Events.State
import Events.State.Types
import Graphics.Vty.Input.Events (Event(EvKey), Key(KEnter, KEsc))
import qualified UI.Field as F (event)

event :: Event -> Stateful
event (EvKey KEnter _) s = case mode s of
    Insert IList ICreate _ -> (write =<<) . (startCreate =<<) . (newItem =<<) . (store =<<) $ createList s
    Insert IList IEdit _ -> (write =<<) . (normalMode =<<) $ finishListTitle s

    Insert ITask ICreate _ -> (write =<<) . (below =<<) . (removeBlank =<<) . (store =<<) $ finishTask s
    Insert ITask IEdit _ -> (write =<<) . (removeBlank =<<) . (normalMode =<<) $ finishTask s
    _ -> return s

event (EvKey KEsc _) s = case mode s of
    Insert IList ICreate _ -> (normalMode =<<) . (write =<<) $ createList s
    Insert IList IEdit _ -> (write =<<) . (normalMode =<<) $ finishListTitle s
    Insert ITask _ _ -> (write =<<) . (removeBlank =<<) . (normalMode =<<) $ finishTask s
    _ -> return s

event e s = return $ case mode s of
    Insert iType iMode field -> s { mode = Insert iType iMode (F.event e field) }
    _ -> s
