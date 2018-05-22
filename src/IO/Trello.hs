{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.Trello (
    TrelloToken
  , TrelloBoardID
  , getCards
) where

import ClassyPrelude

import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, getResponseStatusCode)
import Data.Aeson

import IO.Trello.List (List, trelloListToList, cards)
import IO.Trello.Card (Card, idChecklists, setChecklists)
import IO.Trello.ChecklistItem (ChecklistItem, checkItems)
import Data.Taskell.Lists (Lists)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)

type TrelloToken = Text
type TrelloBoardID = Text
type TrelloChecklistID = Text

key :: Text
key = "80dbcf6f88f62cc5639774e13342c20b"

root :: Text
root = "https://api.trello.com/1/"

fullURL :: Text -> TrelloToken -> String
fullURL uri token = unpack $ concat [root, uri, "&key=", key, "&token=", token]

boardURL :: TrelloBoardID -> TrelloToken -> String
boardURL board = fullURL $ concat [
        "boards/", board, "/lists",
        "?cards=open",
        "&card_fields=name,due,desc,idChecklists",
        "&fields=id,name,cards"
    ]

checklistURL :: TrelloChecklistID -> TrelloToken -> String
checklistURL checklist = fullURL $ concat [
        "checklists/", checklist,
        "?fields=id",
        "&checkItem_fields=name,state"
    ]

trelloListsToLists :: TimeZone -> [List] -> Lists
trelloListsToLists tz ls = fromList $ trelloListToList tz <$> ls

fetch :: String -> IO (Int, ByteString)
fetch url = do
    request <- parseRequest url
    response <- httpBS request
    return (getResponseStatusCode response, getResponseBody response)

getChecklist :: TrelloToken -> TrelloChecklistID -> IO (Either Text [ChecklistItem])
getChecklist token checklist = do
    (status, body) <- fetch (checklistURL checklist token)

    return $ case status of
        200 -> case checkItems <$> decodeStrict body of
            Just ls -> Right ls
            Nothing -> Left "Could not parse response. Please file an Issue on GitHub."
        429 -> Left "Too many checklists"
        _ -> Left $ tshow status ++ " error while fetching checklist " ++ checklist

updateCard :: TrelloToken -> Card -> IO (Either [Text] Card)
updateCard token card = do
    let ids = idChecklists card
    checklists <- sequence $ getChecklist token <$> ids
    return $ case lefts checklists of
        [] -> do
            let cl = foldl' (++) [] $ rights checklists
            Right $ setChecklists cl card
        ls -> Left ls

updateList :: TrelloToken -> List -> IO (Either [Text] List)
updateList token l = do
    cs <- sequence $ updateCard token <$> cards l
    return $ case lefts cs of
        [] -> Right $ l { cards = rights cs }
        ls -> Left $ concat ls

getChecklists :: TrelloToken -> [List] -> IO (Either [Text] [List])
getChecklists token ls = do
    lists <- sequence $ updateList token <$> ls
    return $ case lefts lists of
        [] -> Right $ rights lists
        ls' -> Left $ concat ls'

getCards :: TrelloToken -> TrelloBoardID -> IO (Either Text Lists)
getCards token board = do
    (status, body) <- fetch (boardURL board token)
    timezone <- getCurrentTimeZone

    putStrLn "Loading from Trello..."

    case status of
        200 -> case decodeStrict body of
            Just raw -> do
                lists <- getChecklists token raw
                case lists of
                    Right ls -> return $ Right (trelloListsToLists timezone ls)
                    Left t -> return .Left $ unlines t
            Nothing -> return $ Left "Could not parse response. Please file an Issue on GitHub."
        404 -> return . Left $ "Could not find Trello board " ++ board ++ ". Make sure the ID is correct"
        401 -> return . Left $ "You do not have permission to view Trello board " ++ board
        _ -> return . Left $ tshow status ++ " error. Cannot fetch from Trello."
