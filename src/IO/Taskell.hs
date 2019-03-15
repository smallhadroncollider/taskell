{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module IO.Taskell where

import ClassyPrelude

import Data.FileEmbed   (embedFile)
import System.Directory (doesFileExist, getCurrentDirectory)

import Config             (usage, version)
import Data.Taskell.Lists (Lists, analyse, initial)

import           IO.Config         (Config, general, github, trello)
import           IO.Config.General (filename)
import qualified IO.Config.GitHub  as GitHub (token)
import qualified IO.Config.Trello  as Trello (token)

import IO.Markdown (parse, stringify)

import qualified IO.HTTP.GitHub as GitHub (GitHubIdentifier, getLists)
import qualified IO.HTTP.Trello as Trello (TrelloBoardID, getLists)

import UI.CLI (promptYN)

type ReaderConfig a = ReaderT Config IO a

data Next
    = Output Text
    | Load FilePath
           Lists
    | Exit

parseArgs :: [Text] -> ReaderConfig Next
parseArgs ["-v"]                   = return $ Output version
parseArgs ["-h"]                   = return $ Output usage
parseArgs ["-t", boardID, file]    = loadTrello boardID file
parseArgs ["-g", identifier, file] = loadGitHub identifier file
parseArgs ["-i", file]             = fileInfo file
parseArgs [file]                   = loadFile file
parseArgs []                       = (pack . filename . general <$> ask) >>= loadFile
parseArgs _                        = return $ Output (unlines ["Invalid options", "", usage])

load :: ReaderConfig Next
load = getArgs >>= parseArgs

loadFile :: Text -> ReaderConfig Next
loadFile filepath = do
    mPath <- exists filepath
    case mPath of
        Nothing -> return Exit
        Just path -> do
            content <- readData path
            return $
                case content of
                    Right lists -> Load path lists
                    Left err    -> Output $ pack path ++ ": " ++ err

loadRemote :: (token -> FilePath -> ReaderConfig Next) -> token -> Text -> ReaderConfig Next
loadRemote createFn identifier filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then return $ Output (filepath ++ " already exists")
        else createFn identifier path

loadTrello :: Trello.TrelloBoardID -> Text -> ReaderConfig Next
loadTrello = loadRemote createTrello

loadGitHub :: GitHub.GitHubIdentifier -> Text -> ReaderConfig Next
loadGitHub = loadRemote createGitHub

fileInfo :: Text -> ReaderConfig Next
fileInfo filepath = do
    let path = unpack filepath
    exists' <- fileExists path
    if exists'
        then do
            content <- readData path
            return $
                case content of
                    Right lists -> Output $ analyse filepath lists
                    Left err    -> Output $ pack path ++ ": " ++ err
        else return Exit

createRemote ::
       (Config -> Maybe token)
    -> Text
    -> (token -> ReaderT token IO (Either Text Lists))
    -> token
    -> FilePath
    -> ReaderConfig Next
createRemote tokenFn missingToken getFn identifier path = do
    config <- ask
    let maybeToken = tokenFn config
    case maybeToken of
        Nothing -> return $ Output missingToken
        Just token -> do
            lists <- lift $ runReaderT (getFn identifier) token
            case lists of
                Left txt -> return $ Output txt
                Right ls -> do
                    create <- promptCreate path
                    if create
                        then do
                            lift $ writeData config ls path
                            return $ Load path ls
                        else return Exit

createTrello :: Trello.TrelloBoardID -> FilePath -> ReaderConfig Next
createTrello =
    createRemote
        (Trello.token . trello)
        (decodeUtf8 $(embedFile "templates/trello-token.txt"))
        Trello.getLists

createGitHub :: GitHub.GitHubIdentifier -> FilePath -> ReaderConfig Next
createGitHub =
    createRemote
        (GitHub.token . github)
        (decodeUtf8 $(embedFile "templates/github-token.txt"))
        GitHub.getLists

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
