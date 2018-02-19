module UI.Internal (
    box
) where

import Brick
import UI.Types (ResourceName)
import Data.Text (Text)

box :: [Text] -> Widget ResourceName
box d = padBottom (Pad 1) . vBox $ txt <$> d
