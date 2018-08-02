{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IO.Taskell where

import ClassyPrelude

import System.Directory (getCurrentDirectory, doesFileExist)
import Data.FileEmbed (embedFile)

import Config (version, usage)
import Data.Taskell.Lists (Lists, initial, analyse)
import IO.Config (Config, general, trello, github)
import IO.Config.General (filename)
import qualified IO.Config.Trello as Trello (token)
import qualified IO.Config.GitHub as GitHub (token)
import IO.Markdown (stringify, parse)
import qualified IO.Trello as Trello (TrelloBoardID, getLists)
import qualified IO.GitHub as GitHub (GitHubProjectID, getLists)
import UI.CLI (promptYN)

type ReaderConfig a = ReaderT Config IO a

data Next = Output Text | Load FilePath Lists | Exit

parseArgs :: [Text] -> ReaderConfig Next
parseArgs ["-v"] = return $ Output version
parseArgs ["-h"] = return $ Output usage
parseArgs ["-t", boardID, file] = loadTrello boardID file
parseArgs ["-g", projectID, file] = loadGitHub projectID file
parseArgs ["-i", file] = fileInfo file
parseArgs [file] = loadFile file
parseArgs [] = (pack . filename . general <$> ask) >>= loadFile
parseArgs  _ = return $ Output (unlines ["Invalid options", "", usage])

load :: ReaderConfig Next
load = getArgs >>= parseArgs

loadFile :: Text -> ReaderConfig Next
loadFile filepath = do
    mPath <- exists filepath
    case mPath of
        Nothing -> return Exit
        Just path -> do
            content <- readData path
            return $ case content of
                Right lists -> Load path lists
                Left err -> Output $ pack path ++ ": " ++ err

loadTrello :: Trello.TrelloBoardID -> Text -> ReaderConfig Next
loadTrello boardID filepath = do
    let path = unpack filepath
    exists' <- fileExists path

    if exists'
        then return $ Output (filepath ++ " already exists")
        else createTrello boardID path

loadGitHub :: GitHub.GitHubProjectID -> Text -> ReaderConfig Next
loadGitHub projectID filepath = do
    let path = unpack filepath
    exists' <- fileExists path

    if exists'
        then return $ Output (filepath ++ " already exists")
        else createGitHub projectID path

fileInfo :: Text -> ReaderConfig Next
fileInfo filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then do
            content <- readData path
            return $ case content of
                Right lists -> Output $ analyse filepath lists
                Left err -> Output $ pack path ++ ": " ++ err
        else return Exit

createTrello :: Trello.TrelloBoardID -> FilePath -> ReaderConfig Next
createTrello boardID path = do
    config <- ask
    let maybeToken = Trello.token $ trello config
    case maybeToken of
        Nothing -> return $ Output $ decodeUtf8 $(embedFile "templates/trello-token.txt")
        Just trelloToken -> do
            lists <- lift $ runReaderT (Trello.getLists boardID) trelloToken
            case lists of
                Left txt -> return $ Output txt
                Right ls -> do
                    create <- promptCreate path
                    if create
                        then do
                            lift $ writeData config ls path
                            return $ Load path ls
                        else return Exit

createGitHub :: GitHub.GitHubProjectID -> FilePath -> ReaderConfig Next
createGitHub projectID path = do
    config <- ask
    let maybeToken = GitHub.token $ github config
    case maybeToken of
        Nothing -> return $ Output $ decodeUtf8 $(embedFile "templates/trello-token.txt")
        Just trelloToken -> do
            lists <- lift $ runReaderT (GitHub.getLists projectID) trelloToken
            case lists of
                Left txt -> return $ Output txt
                Right ls -> do
                    create <- promptCreate path
                    if create
                        then do
                            lift $ writeData config ls path
                            return $ Load path ls
                        else return Exit


exists :: Text -> ReaderConfig (Maybe FilePath)
exists filepath = do
    let path = unpack filepath
    exists' <- fileExists path

    if exists'
        then return $ Just path
        else do
            create <- promptCreate path
            if create
                then do
                    createPath path
                    return $ Just path
                else return Nothing

fileExists :: FilePath -> ReaderConfig Bool
fileExists path = lift $ doesFileExist path

promptCreate :: FilePath -> ReaderConfig Bool
promptCreate path = do
    cwd <- lift $ pack <$> getCurrentDirectory
    lift $ promptYN $ concat ["Create ", cwd, "/", pack path, "?"]

-- creates taskell file
createPath :: FilePath -> ReaderConfig ()
createPath path = do
    config <- ask
    lift (writeData config initial path)

-- writes Tasks to json file
writeData :: Config -> Lists -> FilePath -> IO ()
writeData config tasks path = void (writeFile path $ stringify config tasks)

-- reads json file
readData :: FilePath -> ReaderConfig (Either Text Lists)
readData path = do
    config <- ask
    content <- readFile path
    return $ parse config content
