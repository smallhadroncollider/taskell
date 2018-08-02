{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IO.GitHub (
    GitHubToken
  , GitHubProjectID
  , getLists
) where

import ClassyPrelude

import Control.Lens ((^.))

import Data.Aeson

import Network.HTTP.Simple (parseRequest, httpBS, getResponseBody, getResponseStatusCode)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Client (requestHeaders)

import IO.Aeson (parseError)
import IO.GitHub.Column (Column, cardsURL, columnToList)
import IO.GitHub.Card (Card)

import Data.Taskell.Lists (Lists)
import Data.Taskell.List (List)

type GitHubToken = Text
type GitHubProjectID = Text

type ReaderGitHubToken a = ReaderT GitHubToken IO a

root :: Text
root = "https://api.github.com/"

headers :: ReaderGitHubToken [(HeaderName, ByteString)]
headers = do
    token <- ask
    return [
            ("User-Agent", "smallhadroncollider/taskell")
          , ("Accept", "application/vnd.github.inertia-preview+json")
          , ("Authorization", encodeUtf8 ("token " ++ token))
        ]

fetch :: Text -> ReaderGitHubToken (Int, ByteString)
fetch url = do
    initialRequest <- lift $ parseRequest (unpack url)
    rHeaders <- headers
    let request = initialRequest { requestHeaders = rHeaders }
    response <- lift $ httpBS request
    return (getResponseStatusCode response, getResponseBody response)


getCards :: Text -> ReaderGitHubToken (Either Text [Card])
getCards url = do
    (status, body) <- fetch url

    return $ case status of
        200 -> case decodeStrict body of
            Just cards -> Right cards
            Nothing -> Left parseError
        429 -> Left "Too many checklists"
        _ -> Left $ tshow status ++ " error while fetching " ++ url

addCard :: Column -> ReaderGitHubToken (Either Text List)
addCard column = do
    cards <- getCards $ column ^. cardsURL
    return $ columnToList column <$> cards


addCards :: [Column] -> ReaderGitHubToken (Either Text Lists)
addCards columns = do
    cols <- sequence (addCard <$> columns)
    return $ fromList <$> sequence cols


getLists :: GitHubProjectID -> ReaderGitHubToken (Either Text Lists)
getLists project = do
    let url = concat [root, "projects/", project, "/columns"]
    (status, body) <- fetch url

    putStrLn "Fetching from GitHub..."

    case status of
        200 -> case decodeStrict body of
            Just columns -> addCards columns
            Nothing -> return $ Left parseError
        404 -> return . Left $ "Could not find GitHub project " ++ project ++ ". Make sure the ID is correct"
        401 -> return . Left $ "You do not have permission to view GitHub project " ++ project
        _ -> return . Left $ tshow status ++ " error. Cannot fetch from GitHub."
