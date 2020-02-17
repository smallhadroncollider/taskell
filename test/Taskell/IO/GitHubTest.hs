module Taskell.IO.GitHubTest
    ( test_github
    ) where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Taskell.IO.HTTP.GitHub (getNextLink)

-- tests
test_github :: TestTree
test_github =
    testGroup
        "IO.HTTP.GitHub"
        [ testGroup
              "getNextLink"
              [ testCase
                    "next exists"
                    (assertEqual
                         "Parses next link"
                         (Just "https://api.github.com/projects/columns/3152155/cards?page=2")
                         (getNextLink
                              [ "<https://api.github.com/projects/columns/3152155/cards?page=2>; rel=\"next\", <https://api.github.com/projects/columns/3152155/cards?page=2>; rel=\"last\""
                              ]))
              , testCase "empty" (assertEqual "Returns Nothing" Nothing (getNextLink []))
              , testCase
                    "no next"
                    (assertEqual
                         "Returns Nothing"
                         Nothing
                         (getNextLink
                              [ "<https://api.github.com/projects/columns/3152155/cards?page=2>; rel=\"prev\", <https://api.github.com/projects/columns/3152155/cards?page=2>; rel=\"last\""
                              ]))
              , testCase
                    "next last"
                    (assertEqual
                         "Parses next link"
                         (Just "https://api.github.com/projects/columns/3152155/cards?page=2")
                         (getNextLink
                              [ "<https://api.github.com/projects/columns/3152155/cards?page=1>; rel=\"prev\", <https://api.github.com/projects/columns/3152155/cards?page=2>; rel=\"next\", <https://api.github.com/projects/columns/3152155/cards?page=3>; rel=\"last\""
                              ]))
              ]
        ]
