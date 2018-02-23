{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal.Help (
    help
) where

import Brick
import Data.Text as T (Text, lines, replace, breakOn, strip, drop)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)
import IO.Markdown (trimListItem)

import UI.Types (ResourceName)
import UI.Theme (taskCurrentAttr)

help :: Int -> (Text, Widget ResourceName)
help _ = ("Controls", w)
    where ls = T.lines $ decodeUtf8 $(embedFile "templates/controls.md")
          (l, r) = unzip $ breakOn ":" . T.replace "`" "" . trimListItem <$> ls
          left = padRight (Pad 2) . vBox $ withAttr taskCurrentAttr . txt <$> l
          right = vBox $ txt . T.strip . T.drop 1 <$> r
          w = left <+> right
