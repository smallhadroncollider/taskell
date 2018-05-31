{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module IO.Trello.Aeson where

import ClassyPrelude

import GHC.Generics (Rep)
import Data.Aeson (genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (Value, Parser, Zero, GFromJSON)

stripLensPrefix :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
stripLensPrefix = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
