module Draw (
    render
) where

import Graphics.Vty
import Keyboard (isChar)
import Task (Tasks, description)

bullet :: String -> Image
bullet s = string (defAttr ` withForeColor ` magenta) ("- " ++ s)

title = pad 0 0 0 1 (string (defAttr ` withForeColor ` green) "[Taskell]")

-- draws the title
pic :: Tasks -> Picture
pic ts = picForImage $ title <-> imgs
    where imgs = vertCat $ map (bullet . description) ts

-- is it a quit event
quit :: Event -> Bool
quit = isChar 'q'

-- the draw loop
draw :: Vty -> Tasks -> IO ()
draw vty ts = do
     update vty $ pic ts
     e <- nextEvent vty
     
     if quit e
         then shutdown vty
         else draw vty ts

-- setup vty and start the draw loop
render :: Tasks -> IO ()
render ts = do
     cfg <- standardIOConfig
     vty <- mkVty cfg
     draw vty ts
