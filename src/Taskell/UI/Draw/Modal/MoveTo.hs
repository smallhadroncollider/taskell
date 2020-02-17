module Taskell.UI.Draw.Modal.MoveTo
    ( moveTo
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import Taskell.Data.List          (title)
import Taskell.Events.State.Types (current, lists)
import Taskell.Types              (showListIndex)
import Taskell.UI.Draw.Field      (textField)
import Taskell.UI.Draw.Types      (DrawState (dsState), ModalWidget)
import Taskell.UI.Theme           (taskCurrentAttr)

moveTo :: ModalWidget
moveTo = do
    skip <- showListIndex . fst . (^. current) <$> asks dsState
    ls <- toList . (^. lists) <$> asks dsState
    let titles = textField . (^. title) <$> ls
    let letter a =
            padRight (Pad 1) . hBox $
            [txt "[", withAttr taskCurrentAttr $ txt (singleton a), txt "]"]
    let letters = letter <$> ['a' ..]
    let remove i l = take i l <> drop (i + 1) l
    let output (l, t) = l <+> t
    let widget = vBox $ output <$> remove skip (zip letters titles)
    pure ("Move To:", widget)
