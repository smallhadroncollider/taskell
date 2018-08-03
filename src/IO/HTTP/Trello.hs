{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.HTTP.Trello (
    TrelloToken
  , TrelloBoardID
  , getLists
) where

import ClassyPrelude

import Control.Lens ((^.))

import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, getResponseStatusCode)
import Data.Aeson

import IO.HTTP.Aeson (parseError)
import IO.HTTP.Trello.List (List, listToList, setCards, cards)
import IO.HTTP.Trello.Card (Card, idChecklists, setChecklists)
import IO.HTTP.Trello.ChecklistItem (ChecklistItem, checkItems)

import Data.Taskell.Lists (Lists)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)


type ReaderTrelloToken a = ReaderT TrelloToken IO a

type TrelloToken = Text
type TrelloBoardID = Text
type TrelloChecklistID = Text

key :: Text
key = "80dbcf6f88f62cc5639774e13342c20b"

root :: Text
root = "https://api.trello.com/1/"

fullURL :: Text -> ReaderTrelloToken String
fullURL uri = do
    token <- ask
    return . unpack $ concat [root, uri, "&key=", key, "&token=", token]

boardURL :: TrelloBoardID -> ReaderTrelloToken String
boardURL board = fullURL $ concat [
        "boards/", board, "/lists",
        "?cards=open",
        "&card_fields=name,due,desc,idChecklists",
        "&fields=name,cards"
    ]

checklistURL :: TrelloChecklistID -> ReaderTrelloToken String
checklistURL checklist = fullURL $ concat [
        "checklists/", checklist,
        "?fields=id",
        "&checkItem_fields=name,state"
    ]

trelloListsToLists :: TimeZone -> [List] -> Lists
trelloListsToLists tz ls = fromList $ listToList tz <$> ls

fetch :: String -> IO (Int, ByteString)
fetch url = do
    request <- parseRequest url
    response <- httpBS request
    return (getResponseStatusCode response, getResponseBody response)

getChecklist :: TrelloChecklistID -> ReaderTrelloToken (Either Text [ChecklistItem])
getChecklist checklist = do
    url <- checklistURL checklist
    (status, body) <- lift $ fetch url

    return $ case status of
        200 -> case (^. checkItems) <$> decodeStrict body of
            Just ls -> Right ls
            Nothing -> Left parseError
        429 -> Left "Too many checklists"
        _ -> Left $ tshow status ++ " error while fetching checklist " ++ checklist

updateCard :: Card -> ReaderTrelloToken (Either Text Card)
updateCard card = (setChecklists card . concat <$>) . sequence <$> checklists
    where checklists = sequence $ getChecklist <$> (card ^. idChecklists)

updateList :: List -> ReaderTrelloToken (Either Text List)
updateList l = (setCards l <$>) . sequence <$> sequence (updateCard <$> (l ^. cards))

getChecklists :: [List] -> ReaderTrelloToken (Either Text [List])
getChecklists ls = sequence <$> sequence (updateList <$> ls)

getLists :: TrelloBoardID -> ReaderTrelloToken (Either Text Lists)
getLists board = do
    putStrLn "Fetching from Trello..."

    url <- boardURL board
    (status, body) <- lift $ fetch url
    timezone <- lift getCurrentTimeZone

    case status of
        200 -> case decodeStrict body of
            Just raw -> fmap (trelloListsToLists timezone) <$> getChecklists raw
            Nothing -> return $ Left parseError
        404 -> return . Left $ "Could not find Trello board " ++ board ++ ". Make sure the ID is correct"
        401 -> return . Left $ "You do not have permission to view Trello board " ++ board
        _ -> return . Left $ tshow status ++ " error. Cannot fetch from Trello."
