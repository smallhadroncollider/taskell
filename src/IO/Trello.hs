{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Trello where

import ClassyPrelude

import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, getResponseStatusCode)
import Data.Aeson

import IO.Trello.List (List, trelloListToList)
import Data.Taskell.Lists (Lists)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)

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

trelloListsToLists :: TimeZone -> [List] -> Lists
trelloListsToLists tz ls = fromList $ trelloListToList tz <$> ls

getCards :: TrelloToken -> TrelloBoardID -> IO (Either Text Lists)
getCards token board = do
    request <- parseRequest $ getUrl token board
    response <- httpBS request
    timezone <- getCurrentTimeZone
    let status = getResponseStatusCode response
    let body = getResponseBody response

    let result
            | status == 200 = case trelloListsToLists timezone <$> decodeStrict body of
                Just ls -> Right ls
                Nothing -> Left "Could not parse response. Please file an Issue on GitHub."
            | status == 404 = Left $ "Could not find Trello board " ++ board ++ ". Make sure the ID is correct"
            | status == 401 = Left $ "You do not have permission to view Trello board " ++ board
            | otherwise = Left $ tshow status ++ " error. Cannot fetch from Trello."

    return result
