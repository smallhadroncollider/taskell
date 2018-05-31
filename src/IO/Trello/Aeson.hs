{-# LANGUAGE NoImplicitPrelude #-}
module IO.Trello.Aeson where

import ClassyPrelude

import Language.Haskell.TH (Name, Q, Dec)
import Data.Aeson (defaultOptions, fieldLabelModifier)
import qualified Data.Aeson.TH as TH (deriveFromJSON)

deriveFromJSON :: Name -> Q [Dec]
deriveFromJSON = TH.deriveFromJSON defaultOptions { fieldLabelModifier = drop 1 }
