{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IO.Taskell where

import ClassyPrelude

import System.Directory (getCurrentDirectory, doesFileExist)
import Data.FileEmbed (embedFile)

import Config (version, usage)
import Data.Taskell.Lists (Lists, initial)
import IO.Config (Config, general, filename, token, trello)
import IO.Markdown (stringify, parse)
import IO.Trello (TrelloBoardID, getCards)
import UI.CLI (promptYN)

type ReaderConfig a = ReaderT Config IO a

data Next = Output Text | Load FilePath Lists | Exit

parseArgs :: [Text] -> ReaderConfig Next
parseArgs ["-v"] = return $ Output version
parseArgs ["-h"] = return $ Output usage
parseArgs ["-t", boardID, file] = loadTrello boardID file
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

loadTrello :: TrelloBoardID -> Text -> ReaderConfig Next
loadTrello boardID filepath = do
    let path = unpack filepath
    exists' <- fileExists path

    if exists'
        then return $ Output (filepath ++ " already exists")
        else createTrello boardID path

createTrello :: TrelloBoardID -> FilePath -> ReaderConfig Next
createTrello boardID path = do
    config <- ask
    let maybeToken = token $ trello config
    case maybeToken of
        Nothing -> return $ Output $ decodeUtf8 $(embedFile "templates/token.txt")
        Just trelloToken -> do
            lists <- lift $ getCards trelloToken boardID
            case lists of
                Nothing ->
                    return $ Output ("Could not fetch Trello board " ++ boardID ++ ". Please make sure you have permission to view it.")
                Just ls -> do
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
