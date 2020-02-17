{-# LANGUAGE TemplateHaskell #-}

module Taskell.Config where

import ClassyPrelude

import Data.FileEmbed (embedFile)

version :: Text
version = "1.9.3"

usage :: Text
usage = decodeUtf8 $(embedFile "templates/usage.txt")
