{-# LANGUAGE NoImplicitPrelude #-}
module Events.Actions.Modal (event) where

import ClassyPrelude

import Graphics.Vty.Input.Events
import Events.State

import qualified Events.Actions.Modal.Help as Help
import qualified Events.Actions.Modal.MoveTo as MoveTo
import qualified Events.Actions.Modal.SubTasks as SubTasks

event :: Event -> Stateful

event e s = case mode s of
    Modal Help -> Help.event e s
    Modal (SubTasks _ _) -> SubTasks.event e s
    Modal MoveTo -> MoveTo.event e s
    _ -> return s
