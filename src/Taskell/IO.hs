module Taskell.IO where

import ClassyPrelude

import Control.Monad.Reader (runReader)
import Data.Text.Encoding   (decodeUtf8With)
import System.Directory     (doesFileExist, doesDirectoryExist, canonicalizePath, makeRelativeToCurrentDirectory)
import Data.Either (fromRight)

import Data.Time.Zones (TZ)

import Taskell.Config     (githubUsage, trelloUsage, usage, version)
import Taskell.Data.Lists (Lists, analyse, initial)

import           Taskell.IO.Config         (Config, general, getDir, github, markdown, templatePath,
                                            trello)
import           Taskell.IO.Config.General (filename)
import qualified Taskell.IO.Config.GitHub  as GitHub (token)
import qualified Taskell.IO.Config.Trello  as Trello (token)

import Taskell.IO.Markdown (MarkdownInfo (MarkdownInfo), parse, serialize)

import qualified Taskell.IO.HTTP.GitHub as GitHub (GitHubIdentifier, getLists)
import qualified Taskell.IO.HTTP.Trello as Trello (TrelloBoardID, getLists)

import Taskell.UI.CLI (PromptYN (PromptYes), promptYN)

data IOInfo = IOInfo
    { ioTZ     :: TZ
    , ioConfig :: Config
    }

type ReaderConfig a = ReaderT IOInfo IO a

data Next
    = Output Text
    | Error Text
    | Load FilePath
           Lists
    | Exit


getPath :: Text -> ReaderConfig FilePath
getPath path = do
    config <- asks ioConfig
    canonicial <- lift $ canonicalizePath (unpack path)
    let defaultFilename = filename (general config)
    isDir <- lift $ doesDirectoryExist canonicial
    pure $ if isDir then canonicial </> defaultFilename else canonicial

parseArgs :: [Text] -> ReaderConfig Next
parseArgs ["-v"]                   = pure $ Output version
parseArgs ["-h"]                   = pure $ Output usage
parseArgs ["-t", boardID, file]    = getPath file >>= loadTrello boardID
parseArgs ["-g", identifier, file] = getPath file >>= loadGitHub identifier
parseArgs ["-i", file]             = getPath file >>= fileInfo
parseArgs [file]                   = getPath file >>= loadFile
parseArgs []                       = getPath "" >>= loadFile
parseArgs _                        = pure $ Error (unlines ["Invalid options", "", usage])

load :: ReaderConfig Next
load = getArgs >>= parseArgs

colonic :: FilePath -> Text -> Text
colonic path = ((pack path <> ": ") <>)

createLoad :: FilePath -> Lists -> ReaderConfig Next
createLoad path lists = do
    relative <- lift $ makeRelativeToCurrentDirectory path
    pure $ Load relative lists

loadFile :: FilePath -> ReaderConfig Next
loadFile filepath = do
    mPath <- exists filepath
    case mPath of
        Nothing   -> pure Exit
        Just path -> do
            lists <- readData path
            case lists of
                Left err -> pure $ Error (colonic path err)
                Right ls -> createLoad path ls

loadRemote :: (token -> FilePath -> ReaderConfig Next) -> token -> FilePath -> ReaderConfig Next
loadRemote createFn identifier path = do
    exists' <- fileExists path
    if exists'
        then pure $ Error (pack path <> " already exists")
        else createFn identifier path

loadTrello :: Trello.TrelloBoardID -> FilePath -> ReaderConfig Next
loadTrello = loadRemote createTrello

loadGitHub :: GitHub.GitHubIdentifier -> FilePath -> ReaderConfig Next
loadGitHub = loadRemote createGitHub

fileInfo :: FilePath -> ReaderConfig Next
fileInfo path = do
    exists' <- fileExists path
    if exists'
        then either (Error . colonic path) (Output . analyse (pack path)) <$> readData path
        else pure $ Error (pack path <> " does not exist")

createRemote ::
       (Config -> Maybe token)
    -> Text
    -> (token -> ReaderT token IO (Either Text Lists))
    -> token
    -> FilePath
    -> ReaderConfig Next
createRemote tokenFn missingToken getFn identifier path = do
    config <- asks ioConfig
    tz <- asks ioTZ
    case tokenFn config of
        Nothing -> pure $ Error missingToken
        Just token -> do
            lists <- lift $ runReaderT (getFn identifier) token
            case lists of
                Left txt -> pure $ Error txt
                Right ls -> do
                    promptCreate path >>= bool (pure Exit) (lift (writeData tz config ls path) >> createLoad path ls)

createTrello :: Trello.TrelloBoardID -> FilePath -> ReaderConfig Next
createTrello = createRemote (Trello.token . trello) trelloUsage Trello.getLists

createGitHub :: GitHub.GitHubIdentifier -> FilePath -> ReaderConfig Next
createGitHub = createRemote (GitHub.token . github) githubUsage GitHub.getLists

exists :: FilePath -> ReaderConfig (Maybe FilePath)
exists path = do
    exists' <- fileExists path
    if exists'
        then pure $ Just path
        else promptCreate path >>= bool (pure Nothing) (Just path <$ createPath path)

fileExists :: FilePath -> ReaderConfig Bool
fileExists path = lift $ doesFileExist path

promptCreate :: FilePath -> ReaderConfig Bool
promptCreate path = lift $ promptYN PromptYes $ concat ["Create ", pack path, "?"]

-- creates taskell file
createPath :: FilePath -> ReaderConfig ()
createPath path = do
    config <- asks ioConfig
    tz <- asks ioTZ
    template <- readData . templatePath =<< lift getDir
    let ls = fromRight initial template
    lift (writeData tz config ls path)

-- writes Tasks to json file
writeData :: TZ -> Config -> Lists -> FilePath -> IO ()
writeData tz config tasks path = void (writeFile path output)
  where
    output = encodeUtf8 $ runReader (serialize tasks) (MarkdownInfo tz (markdown config))

-- reads json file
decodeError :: String -> Maybe Word8 -> Maybe Char
decodeError _ _ = Just '\65533'

readData :: FilePath -> ReaderConfig (Either Text Lists)
readData path =
    parse <$> (markdown <$> asks ioConfig) <*> (decodeUtf8With decodeError <$> readFile path)
