module UI.Internal (
    box
) where

import Brick
import UI.Types (ResourceName)
import Data.Text (Text)

box :: Int -> [Text] -> Widget ResourceName
box pad d = padBottom (Pad pad) . vBox $ txt <$> d
