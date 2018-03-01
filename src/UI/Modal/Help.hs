{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.Help (
    help
) where

import Brick
import Data.Text as T (Text, lines, replace, breakOn, strip, drop, length, justifyRight)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)
import IO.Markdown (trimListItem)
import Data.Foldable (foldl')

import UI.Types (ResourceName)
import UI.Theme (taskCurrentAttr)
import UI.Field (textField)

line :: Int -> (Text, Text) -> Widget ResourceName
line m (l, r) = left <+> right
    where left = padRight (Pad 2) . withAttr taskCurrentAttr . txt $ justifyRight m ' ' l
          right = textField . T.strip . T.drop 1 $ r

help :: (Text, Widget ResourceName)
help = ("Controls", w)
    where ls = T.lines $ decodeUtf8 $(embedFile "templates/controls.md")
          (l, r) = unzip $ breakOn ":" . T.replace "`" "" . trimListItem <$> ls
          m = foldl' max 0 $ T.length <$> l
          w = vBox $ line m <$> zip l r
