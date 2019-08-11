{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Modal.MoveTo
    ( moveTo
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import Data.Taskell.List  (title)
import Events.State.Types (current, lists)
import UI.Draw.Field      (textField)
import UI.Draw.Types      (DrawState (dsState), ReaderDrawState)
import UI.Theme           (taskCurrentAttr)
import UI.Types           (ResourceName)

moveTo :: ReaderDrawState (Text, Widget ResourceName)
moveTo = do
    skip <- fst . (^. current) <$> asks dsState
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
