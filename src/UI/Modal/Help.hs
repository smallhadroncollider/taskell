{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Modal.Help
    ( help
    ) where

import ClassyPrelude

import Brick
import Data.FileEmbed (embedFile)
import Data.Text      as T (breakOn, justifyRight, replace, strip)

import UI.Field (textField)
import UI.Theme (taskCurrentAttr)
import UI.Types (ResourceName)

line :: Int -> (Text, Text) -> Widget ResourceName
line m (l, r) = left <+> right
  where
    left = padRight (Pad 2) . withAttr taskCurrentAttr . txt $ justifyRight m ' ' l
    right = textField . T.strip . drop 1 $ r

help :: (Text, Widget ResourceName)
help = ("Controls", w)
  where
    ls = lines $ decodeUtf8 $(embedFile "templates/controls.md")
    (l, r) = unzip $ breakOn ":" . T.replace "`" "" . T.strip . drop 1 <$> ls
    m = foldl' max 0 $ length <$> l
    w = vBox $ line m <$> zip l r
