{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module IO.Aeson where

import ClassyPrelude

import Language.Haskell.TH (Name, Q, Dec)
import Data.Aeson (defaultOptions, fieldLabelModifier)
import qualified Data.Aeson.TH as TH (deriveFromJSON)

import Data.FileEmbed (embedFile)

deriveFromJSON :: Name -> Q [Dec]
deriveFromJSON = TH.deriveFromJSON defaultOptions { fieldLabelModifier = drop 1 }

parseError :: Text
parseError = decodeUtf8 $(embedFile "templates/api-error.txt")
