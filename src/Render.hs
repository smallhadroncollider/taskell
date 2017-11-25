module Render (
    render
) where

import Graphics.Vty (Vty, standardIOConfig, mkVty, nextEvent, update, shutdown)

import Flow.State (State, Mode(Shutdown), tasks, mode)
import Flow.Actions (event)
import Persistence.Taskell (writeJSON)
import UI.Main (pic)

-- the draw loop
loop :: Vty -> FilePath-> State -> IO ()
loop vty path state = do
    update vty $ pic state
    e <- nextEvent vty
    
    -- get updated version of state
    let state' = event e state

    -- update json if tasks changed
    if tasks state == tasks state'
        then return ()
        else writeJSON (tasks state') path
    
    -- if event wasn't quit keep going, otherwise shutdown
    case mode state' of 
        Shutdown -> shutdown vty
        _ -> loop vty path state'

-- setup vty and start the loop
render :: FilePath -> State -> IO ()
render path state = do
    vty <- standardIOConfig >>= mkVty
    loop vty path state
