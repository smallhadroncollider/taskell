module Lib (
    render
) where

import Graphics.Vty
import Keyboard

-- draws the title
pic :: Picture
pic = picForImage title
    where title = string (defAttr ` withForeColor ` green) ("[Taskell]")

-- is it a quit event
quit :: Event -> Bool
quit = isChar 'q'

-- whether to quit or not
next :: Event -> (Vty -> IO ())
next e = if quit e then shutdown else draw

-- the draw loop
draw :: Vty -> IO ()
draw vty = do
     update vty pic
     e <- nextEvent vty
     next e vty

-- setup vty and start the draw loop
render :: IO ()
render = do
     cfg <- standardIOConfig
     vty <- mkVty cfg
     draw vty
