{-# LANGUAGE TemplateHaskell #-}

module Taskell.Config where

import ClassyPrelude

import           Data.FileEmbed             (embedFile)
import           Data.Version               (showVersion)
import           Language.Haskell.TH.Syntax (liftString)
import qualified Paths_taskell              (version)

version :: Text
version = $(liftString $ showVersion Paths_taskell.version)

usage :: Text
usage = decodeUtf8 $(embedFile "templates/usage.txt")
