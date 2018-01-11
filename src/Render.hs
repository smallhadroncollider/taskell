module Render (
    render
) where

import Graphics.Vty (Vty, standardIOConfig, mkVty, nextEvent, update, shutdown)

import Flow.State (State, Mode(Write, Shutdown), lists, mode, continue)
import Flow.Actions (event)
import Persistence.Taskell (writeJSON)
import UI.Main (pic)

write :: FilePath -> State -> IO State
write path s = case mode s of
    (Write _) -> writeJSON (lists s) path >> return (continue s)
    _ -> return s

-- the draw loop
loop :: Vty -> FilePath-> State -> IO ()
loop vty path state = do
    update vty $ pic state
    e <- nextEvent vty

    -- get updated version of state
    state' <- write path $ event e state

    -- if event wasn't quit keep going, otherwise shutdown
    case mode state' of
        Shutdown -> shutdown vty
        _ -> loop vty path state'

-- setup vty and start the loop
render :: FilePath -> State -> IO ()
render path state = do
    vty <- standardIOConfig >>= mkVty
    loop vty path state
