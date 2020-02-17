module Taskell.IO.GitHub.CardsTest
    ( test_cards
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson

import Taskell.IO.HTTP.GitHub.Card (MaybeCard (MaybeCard))

decodeCards :: Text -> Either String [MaybeCard]
decodeCards txt = eitherDecodeStrict $ encodeUtf8 txt

-- tests
test_cards :: TestTree
test_cards =
    testGroup
        "IO.HTTP.GitHub.Card"
        [ testCase
              "parses basic card"
              (assertEqual
                   "Gives back card"
                   (Right [MaybeCard (Just "blah") Nothing])
                   (decodeCards "[{\"note\": \"blah\"}]"))
        , testCase
              "parses basic card"
              (assertEqual
                   "Gives back card"
                   (Right
                        [MaybeCard Nothing (Just "https://api.github.com/projects/columns/7850783")])
                   (decodeCards
                        "[{\"note\": null, \"content_url\": \"https://api.github.com/projects/columns/7850783\"}]"))
        ]
