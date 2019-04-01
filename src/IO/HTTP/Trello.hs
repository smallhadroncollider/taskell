{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.HTTP.Trello
    ( TrelloToken
    , TrelloBoardID
    , getLists
    ) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Aeson
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpBS, parseRequest)

import IO.HTTP.Aeson                (parseError)
import IO.HTTP.Trello.Card          (Card, idChecklists, setChecklists)
import IO.HTTP.Trello.ChecklistItem (ChecklistItem, checkItems)
import IO.HTTP.Trello.List          (List, cards, listToList, setCards)

import Data.Taskell.Lists  (Lists)
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
    pure . unpack $ concat [root, uri, "&key=", key, "&token=", token]

boardURL :: TrelloBoardID -> ReaderTrelloToken String
boardURL board =
    fullURL $
    concat
        [ "boards/"
        , board
        , "/lists"
        , "?cards=open"
        , "&card_fields=name,due,desc,idChecklists"
        , "&fields=name,cards"
        ]

checklistURL :: TrelloChecklistID -> ReaderTrelloToken String
checklistURL checklist =
    fullURL $ concat ["checklists/", checklist, "?fields=id", "&checkItem_fields=name,state"]

trelloListsToLists :: TimeZone -> [List] -> Lists
trelloListsToLists tz ls = fromList $ listToList tz <$> ls

fetch :: String -> IO (Int, ByteString)
fetch url = do
    request <- parseRequest url
    response <- httpBS request
    pure (getResponseStatusCode response, getResponseBody response)

getChecklist :: TrelloChecklistID -> ReaderTrelloToken (Either Text [ChecklistItem])
getChecklist checklist = do
    url <- checklistURL checklist
    (status, body) <- lift $ fetch url
    pure $
        case status of
            200 ->
                case (^. checkItems) <$> eitherDecodeStrict body of
                    Right ls -> Right ls
                    Left err -> Left $ parseError err
            429 -> Left "Too many checklists"
            _ -> Left $ tshow status <> " error while fetching checklist " <> checklist

updateCard :: Card -> ReaderTrelloToken (Either Text Card)
updateCard card = (setChecklists card . concat <$>) . sequence <$> checklists
  where
    checklists = traverse getChecklist (card ^. idChecklists)

updateList :: List -> ReaderTrelloToken (Either Text List)
updateList l = (setCards l <$>) . sequence <$> traverse updateCard (l ^. cards)

getChecklists :: [List] -> ReaderTrelloToken (Either Text [List])
getChecklists ls = sequence <$> traverse updateList ls

getLists :: TrelloBoardID -> ReaderTrelloToken (Either Text Lists)
getLists board = do
    putStrLn "Fetching from Trello..."
    url <- boardURL board
    (status, body) <- lift $ fetch url
    timezone <- lift getCurrentTimeZone
    case status of
        200 ->
            case eitherDecodeStrict body of
                Right raw -> fmap (trelloListsToLists timezone) <$> getChecklists raw
                Left err  -> pure . Left $ parseError err
        404 ->
            pure . Left $ "Could not find Trello board " <> board <> ". Make sure the ID is correct"
        401 -> pure . Left $ "You do not have permission to view Trello board " <> board
        _ -> pure . Left $ tshow status <> " error. Cannot fetch from Trello."
