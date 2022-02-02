module Taskell.IO where

import ClassyPrelude

import Control.Monad.Reader (runReader)
import Data.Text.Encoding   (decodeUtf8With)
import System.Directory     (doesFileExist, doesDirectoryExist, canonicalizePath, makeRelativeToCurrentDirectory)
import Data.Either (fromRight)

import Data.Time.Zones (TZ)

import Options.Applicative (Parser, ParserInfo, ParserPrefs (..),
                            switch, strOption, strArgument, long, short, metavar, help,
                            info, helper, headerDoc, customExecParser, defaultPrefs)

import Taskell.Config     (githubUsage, trelloUsage, version)
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

-- | Parse command-line arguments to an action
commandLineArgsParser :: Parser (ReaderConfig Next)
commandLineArgsParser = foldl' (<|>) noArgs
    [ openBoardFile <$> fileParser

    , pure (Output version) <$
          switch (short 'v' <> long "version" <> help "Show version number")

    , (\boardID file -> getPath file >>= loadTrello boardID)
          <$> strOption
                  ( short 't'
                  <> long "trello"
                  <> metavar "<trello-board-id>"
                  <> help "Create a new taskell file from the given Trello board ID"
                  )
          <*> fileParser

    , (\identifier file -> getPath file >>= loadGitHub identifier)
          <$> strOption
                  ( short 'g'
                  <> long "github"
                  <> metavar "[orgs/<org> | repos/<username>/<repo>]"
                  <> help "Create a new taskell file from the given GitHub identifier"
                  )
          <*> fileParser

    , fmap (getPath >=> fileInfo) $
          switch
              ( short 'i'
              <> long "info"
              <> help "Display information about a file"
              )
          *> fileParser
    ]
    where
        fileParser = strArgument $ metavar "file"
        openBoardFile = getPath >=> loadFile

        -- | Open default board file
        noArgs = pure $ openBoardFile mempty

commandLineArgsParserInfo :: ParserInfo (ReaderConfig Next)
commandLineArgsParserInfo =
    info (helper <*> commandLineArgsParser) $
        headerDoc (Just "Taskell - A CLI kanban board/task manager")

load :: ReaderConfig Next
load =
    join . liftIO $
        customExecParser
            defaultPrefs { prefShowHelpOnError = True }
            commandLineArgsParserInfo

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
