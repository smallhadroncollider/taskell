{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Modal (
    showModal
) where

import Events.State (State, Mode(..), ModalType(..), mode)
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)

import UI.Types (ResourceName(..))

modal :: Text -> Widget ResourceName
modal = centerLayer . border . padTopBottom 1 . padLeftRight 4 . txt

help :: Widget ResourceName
help = modal $ decodeUtf8 $(embedFile "templates/help.txt")

getModal :: ModalType -> Widget ResourceName
getModal t = case t of
    Help -> help

showModal :: State -> [Widget ResourceName] -> [Widget ResourceName]
showModal s view = case mode s of
    Modal t -> getModal t : view
    _ -> view
