{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.MoveTo
    ( moveTo
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Brick

import Data.Taskell.List (title)
import UI.Draw.Types     (DrawState (dsCurrent, dsLists), ReaderDrawState)
import UI.Field          (textField)
import UI.Theme          (taskCurrentAttr)
import UI.Types          (ResourceName)

moveTo :: ReaderDrawState (Text, Widget ResourceName)
moveTo = do
    skip <- fst <$> asks dsCurrent
    ls <- toList <$> asks dsLists
    let titles = textField . (^. title) <$> ls
    let letter a =
            padRight (Pad 1) . hBox $
            [txt "[", withAttr taskCurrentAttr $ txt (singleton a), txt "]"]
    let letters = letter <$> ['a' ..]
    let remove i l = take i l <> drop (i + 1) l
    let output (l, t) = l <+> t
    let widget = vBox $ output <$> remove skip (zip letters titles)
    pure ("Move To:", widget)
