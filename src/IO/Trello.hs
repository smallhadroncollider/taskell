{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Trello where

import ClassyPrelude

import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody)
import Data.Aeson

import IO.Trello.List (List, trelloListToList)
import Data.Taskell.Lists (Lists)

type TrelloToken = Text
type TrelloBoardID = Text

key :: Text
key = "80dbcf6f88f62cc5639774e13342c20b"

root :: Text
root = "https://api.trello.com/1/boards/"

getUrl :: TrelloToken -> TrelloBoardID -> String
getUrl token board = unpack $ concat [
        root,
        board,
        "/lists",
        "?cards=all",
        "&card_fields=name,due,desc",
        "&fields=id,name,cards",
        "&key=",
        key,
        "&token=",
        token
    ]

trelloListsToLists :: [List] -> Lists
trelloListsToLists ls = fromList $ trelloListToList <$> ls

getCards :: TrelloToken -> TrelloBoardID -> IO (Maybe Lists)
getCards token board = do
    request <- parseRequest $ getUrl token board
    response <- getResponseBody <$> httpBS request
    return $ trelloListsToLists <$> decodeStrict response
