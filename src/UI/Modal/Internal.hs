module UI.Modal.Internal (
    surround
) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.Text as T (Text)
import Data.Taskell.Text (wrap)

import UI.Types (ResourceName(..))
import UI.Theme (titleAttr)

import UI.Internal (box)

maxWidth :: Int
maxWidth = 50

modal :: Int -> (Text, Widget ResourceName) -> Widget ResourceName
modal width (title, w) = t <=> w'
    where t = padBottom (Pad 1) . withAttr titleAttr $ box (wrap width title)
          w'= viewport RNModal Vertical w

surround :: (Int -> (Text, Widget ResourceName)) -> Widget ResourceName
surround fn = Widget Fixed Fixed $ do
    ctx <- getContext

    let fullWidth = availWidth ctx
        padding = 4
        w = (if fullWidth > maxWidth then maxWidth else fullWidth) - (padding * 2)
        widget = modal w $ fn w

    render
        . padTopBottom 1
        . centerLayer
        . border
        . padTopBottom 1
        . padLeftRight padding
        . hLimit w
        $ widget
