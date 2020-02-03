{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module IO.HTTP.GitHub
    ( GitHubToken
    , GitHubIdentifier
    , getNextLink
    , getLists
    ) where

import ClassyPrelude

import Control.Lens  ((^.))
import Data.Either   (lefts, rights)
import Data.Sequence (mapWithIndex, (!?))
import Data.Text     (splitOn, strip)

import Data.Aeson
import UI.CLI     (prompt)

import Network.HTTP.Client       (requestHeaders)
import Network.HTTP.Simple       (getResponseBody, getResponseHeader, getResponseStatusCode, httpBS,
                                  parseRequest)
import Network.HTTP.Types.Header (HeaderName)

import IO.HTTP.Aeson          (parseError)
import IO.HTTP.GitHub.Card    (MaybeCard, content_url, maybeCardToTask)
import IO.HTTP.GitHub.Column  (Column, cardsURL, columnToList)
import IO.HTTP.GitHub.Issue   (issueToTask)
import IO.HTTP.GitHub.Project (Project, columnsURL, name)

import Data.Taskell.List  (List)
import Data.Taskell.Lists (Lists)
import Data.Taskell.Task  (Task)

type GitHubToken = Text

type GitHubIdentifier = Text

type ReaderGitHubToken a = ReaderT GitHubToken IO a

concatEithers :: [Either String [a]] -> Either String [a]
concatEithers vals =
    if null errors
        then Right $ concat (rights vals)
        else Left $ unlines errors
  where
    errors = lefts vals

root :: Text
root = "https://api.github.com/"

headers :: ReaderGitHubToken [(HeaderName, ByteString)]
headers = do
    token <- ask
    pure
        [ ("User-Agent", "smallhadroncollider/taskell")
        , ("Accept", "application/vnd.github.inertia-preview+json")
        , ("Authorization", encodeUtf8 ("token " <> token))
        ]

getNextLink :: [ByteString] -> Maybe Text
getNextLink bs = do
    lnks <- splitOn "," . decodeUtf8 <$> headMay bs
    let rel = "rel=\"next\""
    next <- find (isSuffixOf rel) lnks
    stripPrefix "<" =<< stripSuffix (">; " <> rel) (strip next)

fetch' :: [ByteString] -> Text -> ReaderGitHubToken (Int, [ByteString])
fetch' bs url = do
    initialRequest <- lift $ parseRequest (unpack url)
    rHeaders <- headers
    let request = initialRequest {requestHeaders = rHeaders}
    response <- lift $ httpBS request
    let responses = bs <> [getResponseBody response]
    case getNextLink (getResponseHeader "Link" response) of
        Nothing  -> pure (getResponseStatusCode response, responses)
        Just lnk -> fetch' responses lnk

fetch :: Text -> ReaderGitHubToken (Int, [ByteString])
fetch = fetch' []

fetchContent :: MaybeCard -> ReaderGitHubToken (Either Text Task)
fetchContent card =
    case maybeCardToTask card of
        Just tsk -> pure $ Right tsk
        Nothing ->
            case card ^. content_url of
                Nothing -> pure $ Left "Could not parse card"
                Just url -> do
                    (_, body) <- fetch url
                    let iss = headMay body
                    case iss of
                        Nothing -> pure $ Left "Could not find card content"
                        Just is -> pure . first parseError $ issueToTask <$> eitherDecodeStrict is

getCards :: Text -> ReaderGitHubToken (Either Text [Task])
getCards url = do
    (status, body) <- fetch url
    case status of
        200 ->
            case concatEithers (eitherDecodeStrict <$> body) of
                Right cards -> do
                    cds <- sequence (fetchContent <$> cards)
                    let (ls, rs) = partitionEithers cds
                    pure $
                        if null ls
                            then Right rs
                            else Left (unlines ls)
                Left err -> pure $ Left (parseError err)
        429 -> pure $ Left "Too many cards"
        _ -> pure . Left $ tshow status <> " error while fetching " <> url

addCard :: Column -> ReaderGitHubToken (Either Text List)
addCard column = do
    cards <- getCards $ column ^. cardsURL
    pure $ columnToList column <$> cards

addCards :: [Column] -> ReaderGitHubToken (Either Text Lists)
addCards columns = (fromList <$>) . sequence <$> traverse addCard columns

getColumns :: Text -> ReaderGitHubToken (Either Text Lists)
getColumns url = do
    putStrLn "Fetching project from GitHub..."
    (status, body) <- fetch url
    case status of
        200 ->
            case concatEithers (eitherDecodeStrict <$> body) of
                Right columns -> addCards columns
                Left err      -> pure $ Left (parseError err)
        404 -> pure . Left $ "Could not find GitHub project ."
        401 -> pure . Left $ "You do not have permission to view GitHub project " <> url
        _ -> pure . Left $ tshow status <> " error. Cannot fetch columns from GitHub."

printProjects :: Seq Project -> Text
printProjects projects = unlines $ toList display
  where
    names = (^. name) <$> projects
    line i nm = concat ["[", tshow (i + 1), "] ", nm]
    display = line `mapWithIndex` names

chooseProject :: [Project] -> ReaderGitHubToken (Either Text Lists)
chooseProject projects = do
    let projects' = fromList projects
    putStrLn $ printProjects projects'
    chosen <- lift $ prompt "Import project"
    let project = (projects' !?) =<< (-) 1 <$> readMay chosen
    case project of
        Nothing   -> pure $ Left "Invalid project selected"
        Just proj -> getColumns (proj ^. columnsURL)

getLists :: GitHubIdentifier -> ReaderGitHubToken (Either Text Lists)
getLists identifier = do
    putStrLn "Fetching project list from GitHub...\n"
    let url = concat [root, identifier, "/projects"]
    (status, body) <- fetch url
    case status of
        200 ->
            case concatEithers (eitherDecodeStrict <$> body) of
                Right projects ->
                    if null projects
                        then pure . Left $ concat ["\nNo projects found for ", identifier, "\n"]
                        else chooseProject projects
                Left err -> pure $ Left (parseError err)
        404 ->
            pure . Left $
            "Could not find GitHub org/repo. For organisation make sure you use 'orgs/<org-name>' and for repos use 'repos/<username>/<repo-name>'"
        401 ->
            pure . Left $
            "You do not have permission to view the GitHub projects for " <> identifier
        _ -> pure . Left $ tshow status <> " error. Cannot fetch projects from GitHub."
