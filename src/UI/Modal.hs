{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode)
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.Text as T (Text, lines, replace, breakOn, strip, drop)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)
import IO.Markdown (trimListItem)

import UI.Types (ResourceName(..))
import UI.Theme (titleAttr, taskCurrentAttr)

place :: Widget ResourceName -> Widget ResourceName
place = centerLayer . border . padTopBottom 1 . padLeftRight 4

modal :: Text -> Widget ResourceName -> Widget ResourceName
modal title w = place $ t <=> w
    where t = padBottom (Pad 1) . withAttr titleAttr $ txt title

help :: Widget ResourceName
help = modal "Controls" w
    where ls = T.lines $ decodeUtf8 $(embedFile "templates/controls.md")
          (l, r) = unzip $ breakOn ":" . T.replace "`" "" . trimListItem <$> ls
          left = padRight (Pad 2) . vBox $ withAttr taskCurrentAttr . txt <$> l
          right = vBox $ txt . T.strip . T.drop 1 <$> r
          w = left <+> right

getModal :: ModalType -> Widget ResourceName
getModal t = case t of
    Help -> help

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal t : view
    _ -> view
