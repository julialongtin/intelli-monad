{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module IntelliMonad.Repl
  (
    callInput
  , lexm
  , parseSessionName
  , runRepl
  )
where

import Prelude (Bool(True), Either(Left, Right), IO, (.), ($), (<>), (>>), (++), (<$>), (>>=), fmap, print, pure, putStrLn, return, show)

import Control.Monad (forM_, mapM_)

import Control.Monad.Fail (MonadFail)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.State (get, put)

import Data.Aeson.Encode.Pretty (encodePretty)

import Data.Maybe (Maybe(Just, Nothing))

import qualified Data.ByteString as BS (putStr, toStrict, writeFile)

import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, pack, unpack)
import qualified Data.Text.IO as T (putStr, putStrLn, readFile)

import qualified Data.Yaml as Y (decodeFileEither)
import qualified Data.Yaml.Pretty as Y (defConfig, encodePretty)

import GHC.IO.Exception (ExitCode(ExitFailure, ExitSuccess))

import qualified Louter.Types.Request as Louter (ChatRequest, reqModel)

import System.Console.Haskeline (InputT, Settings(Settings, autoAddHistory, complete, historyFile), completeFilename, defaultSettings, getExternalPrint, getInputLine, runInputT)

import System.Environment (lookupEnv)

import System.FilePath ((</>))

import System.IO (hClose)

import System.IO.Temp (withSystemTempFile)

import System.Process (system)

import Text.Megaparsec ((<|>), anySingle, choice, empty, errorBundlePretty, many, parse, try)

import Text.Megaparsec.Char (alphaNumChar, char, space1, string)

import Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)

import IntelliMonad.BaseTypes (ChatCompletion(toRequest), CommandSpec(CommandSpec, cmdSyntax, cmdParser), Content(Content), Contents, Context(contextBody, contextFooter, contextHeader, contextRequest, contextSessionName, contextTotalTokens, contextToolbox), CustomInstructionProxy, Message(Message), MonadTerminal(termOutput), ToolProxy, PersistentBackend(deleteSession, listSessions, load, save), Prompt, PromptEnv(context, extraCommands, inputCallback, outputCallback, timeoutSeconds))

import IntelliMonad.Parser (Parser)

import IntelliMonad.Persist (getDataDir, withDB)

import IntelliMonad.Prompt (callWithImage, callWithText, clear, getContext, push, runPrompt, setContext, showContents)

import IntelliMonad.Config (readConfig)
import qualified IntelliMonad.Config as Config (getUseStreaming)

import IntelliMonad.ToolPolicy (getTools, changeToolPolicy)

import IntelliMonad.ToolPolicy.Types (ToolEntry(ToolEntry), ToolPolicy(Allow,Ask,Deny))

-- Parser helpers
lexm :: Parser a -> Parser a
lexm = lexeme (L.space space1 empty empty)

parseSessionName = many (alphaNumChar <|> char '-')

defaultCommands :: forall p. PersistentBackend p => [CommandSpec]
defaultCommands =
  [
    CommandSpec ":quit"  (try (lexm (string ":quit")) >> pure (return ()))
  , CommandSpec ":clear" (try (lexm (string ":clear")) >> pure (clear @p))
  , CommandSpec ":model <modelname>" (try (lexm (string ":model") >> lexm parseModelName) >>= pure . handleModelName)
  , CommandSpec ":set timeout <seconds>" (try (lexm (string ":set" ) >> lexm (string "timeout") >> L.decimal) >>= pure . handleSetTimeout)
  , CommandSpec ":show contents" (try (lexm (string ":show") >> lexm (string "contents")) >> pure handleShowContents)
  , CommandSpec ":show context" (try (lexm (string ":show") >> lexm (string "context")) >> pure handleShowContext)
  , CommandSpec ":show request" (try (lexm (string ":show") >> lexm (string "request")) >> pure handleShowRequest)
  , CommandSpec ":show session" (try (lexm (string ":show") >> lexm (string "session")) >> pure handleShowSession)
  , CommandSpec ":show usage" (try (lexm (string ":show") >> lexm (string "usage")) >> pure handleShowUsage)
  , CommandSpec ":set tool <toolname> allow" (try (lexm (string ":set" ) >> lexm (string "tool") >> (lexm parseToolName >>= \name -> lexm (string "allow") >> pure (handleSetToolPolicy Allow name)) ))
  , CommandSpec ":set tool <toolname> ask" (try (lexm (string ":set" ) >> lexm (string "tool") >> (lexm parseToolName >>= \name -> lexm (string "ask") >> pure (handleSetToolPolicy Ask name)) ))
  , CommandSpec ":set tool <toolname> deny <denyreason>" (try (lexm (string ":set" ) >> lexm (string "tool") >> (lexm parseToolName >>= \name -> lexm (string "deny") >> lexm (many anySingle) >>= \reason -> pure (handleSetToolPolicy (Deny $ T.pack reason) name)) ))
  , CommandSpec ":read image <imagepath>" (try (lexm (string ":read") >> lexm (string "image") >> lexm parseImagePath) >>= pure . handleReadImage)
  , CommandSpec ":list sessions" (try (lexm (string ":list") >> lexm (string "sessions")) >> pure handleListSessions)
  , CommandSpec ":list tools" (try (lexm (string ":list") >> lexm (string "tools")) >> pure handleListTools)
  , CommandSpec ":copy session <sessionname>" ( try ( lexm (string ":copy") >> lexm (string "session") >> lexm parseSessionName >>= \src -> lexm parseSessionName >>= \dst -> pure (handleCopySession src dst) ))
  , CommandSpec ":delete session <sessionname>" (try (lexm (string ":delete") >> lexm (string "session") >> lexm parseSessionName) >>= pure . handleDeleteSession)
  , CommandSpec ":switch session <sessionname>" (try (lexm (string ":switch") >> lexm (string "session") >> lexm parseSessionName) >>= pure . handleSwitchSession)
  , CommandSpec ":help" (try (lexm (string ":help")) >> pure (handleHelp @p))
  , CommandSpec ":edit request" (try (lexm (string ":edit") >> lexm (string "request")) >> pure handleEditRequest)
  , CommandSpec ":edit contents" (try (lexm (string ":edit") >> lexm (string "contents")) >> pure handleEditContents)
  , CommandSpec ":edit header" (try (lexm (string ":edit") >> lexm (string "header")) >> pure handleEditHeader)
  , CommandSpec ":edit footer" (try (lexm (string ":edit") >> lexm (string "footer")) >> pure handleEditFooter)
  , CommandSpec ":edit" (try (lexm (string ":edit")) >> pure handleEdit)
  ]
  where
    parseImagePath = many (alphaNumChar <|> char '.' <|> char '/' <|> char '-')
    parseModelName = many (alphaNumChar <|> char '-' <|> char '.' <|> char ':' <|> char '/')
    parseToolName = many (alphaNumChar <|> char '-' <|> char '.' <|> char ':' <|> char '/' <|> char '_')

    handleModelName modelName = do
      prev <- getContext
      let req = prev.contextRequest { Louter.reqModel = T.pack modelName }
          newContext = prev {contextRequest = req}
      setContext @p newContext
      termOutput $ "Model set to: " <> T.pack modelName <> "\n"

    handleSetTimeout timeout = do
      env <- get
      put $ env { timeoutSeconds = Just timeout }
      liftIO $ T.putStrLn $ "Timeout set to: " <> T.pack (show timeout) <> " seconds"

    handleSetToolPolicy policy toolName = do
      prev <- getContext
      let
        maybeNewRegistry = changeToolPolicy prev.contextToolbox (T.pack toolName) policy
      case maybeNewRegistry of
        Nothing -> do
          liftIO $ T.putStrLn $ "Tool Policy set failed. Could not find tool: " <> T.pack toolName <> "."
        Just newRegistry -> do
          liftIO $ T.putStrLn $ "\"" <> T.pack toolName <> "\" policy set to: " <> T.pack (show policy)
          env <- get
          put $ env { context = prev { contextToolbox = newRegistry } }

    handleShowContents = do
      context <- getContext
      showContents context.contextBody

    handleShowUsage = do
      context <- getContext
      liftIO $ do
        print context.contextTotalTokens

    handleShowRequest = do
      context <- getContext
      let req = toRequest context.contextRequest (context.contextHeader <> context.contextBody <> context.contextFooter)
      liftIO $ do
        BS.putStr $ BS.toStrict $ encodePretty req
        T.putStrLn ""

    handleShowContext = do
      prev <- getContext
      liftIO $ do
        putStrLn $ show prev

    handleShowSession = do
      prev <- getContext
      liftIO $ do
        T.putStrLn $ prev.contextSessionName

    handleListSessions = do
      liftIO $ do
        list <- withDB @p $ \conn -> listSessions @p conn
        forM_ list $ \sessionName' -> T.putStrLn sessionName'

    handleListTools = do
      context <- getContext
      liftIO $ do
        putStrLn $ "Policy | Name -- Description" <> "\n" <> "----------------------------"
        let
          list = getTools context
          showToolEntry (name, (ToolEntry desc policy)) =
            case policy of
              Allow -> T.putStrLn $ "ALLOW  | " <> name <> " -- " <> desc
              Ask -> T.putStrLn $ "ASK    | " <> name <> " -- " <> desc
              (Deny reason) -> T.putStrLn $ "DENY   | " <> name <> " -- " <> desc <> " -- Deny reason: " <> reason
        forM_ list $ \a -> showToolEntry a

    handleCopySession src dest =
      let from' = T.pack src
          to' = T.pack dest
      in liftIO $ do
        withDB @p $ \conn -> do
          mv <- load @p conn from'
          case mv of
            Just v -> do
              _ <- save @p conn (v {contextSessionName = to'})
              return ()
            Nothing -> T.putStrLn $ "Failed to load " <> from'

    handleDeleteSession session =
      withDB @p $ \conn -> deleteSession @p conn (T.pack session)

    handleSwitchSession session = do
      mv <- withDB @p $ \conn -> load @p conn (T.pack session)
      case mv of
        Just v -> do
          (env :: PromptEnv) <- get
          put $ env {context = v}
        Nothing -> liftIO $ T.putStrLn $ "Failed to load " <> (T.pack session)

    handleReadImage imagePath =
      callWithImage @p (T.pack imagePath) >>= showContents

    
    handleEdit = do
      -- Open a temporary file with the default editor of the system.
      -- Then send it as user input.
      editWithEditor >>= \case
        Just input -> callInput @p input
        Nothing -> do
          liftIO $ putStrLn "Failed to open the editor."

    handleEditRequest = do
      -- Open a json file of request and edit it with the default editor of the system.
      -- Then, read the file and parse it as a request.
      -- Finally, update the context with the new request.
      prev <- getContext
      let req = toRequest prev.contextRequest (prev.contextHeader <> prev.contextBody <> prev.contextFooter)
      editRequestWithEditor req >>= \case
        Just req' -> do
          let newContext = prev {contextRequest = req'}
          setContext @p newContext
        Nothing -> do
          liftIO $ putStrLn "Failed to open the editor."

    handleEditContents = do
      prev <- getContext
      editContentsWithEditor prev.contextBody >>= \case
        Just contents' -> do
          let newContext = prev {contextBody = contents'}
          setContext @p newContext
        Nothing -> do
          liftIO $ putStrLn "Failed to open the editor."

    handleEditHeader = do
      prev <- getContext
      editContentsWithEditor prev.contextHeader >>= \case
        Just contents' -> do
          let newContext = prev {contextHeader = contents'}
          setContext @p newContext
        Nothing -> do
          liftIO $ putStrLn "Failed to open the editor."

    handleEditFooter = do
      prev <- getContext
      editContentsWithEditor prev.contextFooter >>= \case
        Just contents' -> do
          let newContext = prev {contextFooter = contents'}
          setContext @p newContext
        Nothing -> do
          liftIO $ putStrLn "Failed to open the editor."

    handleHelp :: forall p2. PersistentBackend p2 => Prompt (InputT IO) ()
    handleHelp = do
      env <- get
      liftIO $ mapM_ (T.putStrLn . cmdSyntax) (env.extraCommands ++ defaultCommands @p2)

-- Feed a given string of text to the LLM, and get back a result.
callInput :: forall p. PersistentBackend p => Text -> Prompt (InputT IO) ()
callInput input = do
  config <- liftIO $ readConfig
  if Config.getUseStreaming config
    then do
      liftIO $ T.putStr "assistant: "
      _ <- callWithText @p input
      liftIO $ T.putStrLn ""
    else do
      result <- callWithText @p input
      showContents [con | con@(Content _ (Message _) _ _) <- result]


editWithEditor :: forall m. (MonadIO m, MonadFail m) => m (Maybe Text)
editWithEditor = do
  liftIO $ withSystemTempFile "tempfile.txt" $ \filePath fileHandle -> do
    hClose fileHandle
    editor <- do
      lookupEnv "EDITOR" >>= \case
        Just editor' -> return editor'
        Nothing -> return "vim"
    code <- system (editor <> " " <> filePath)
    case code of
      ExitSuccess -> Just <$> T.readFile filePath
      ExitFailure _ -> return Nothing

editRequestWithEditor :: forall m. (MonadIO m, MonadFail m) => Louter.ChatRequest -> m (Maybe Louter.ChatRequest)
editRequestWithEditor req = do
  liftIO $ withSystemTempFile "tempfile.yaml" $ \filePath fileHandle -> do
    hClose fileHandle
    BS.writeFile filePath $ Y.encodePretty Y.defConfig req
    editor <- do
      lookupEnv "EDITOR" >>= \case
        Just editor' -> return editor'
        Nothing -> return "vim"
    code <- system (editor <> " " <> filePath)
    case code of
      ExitSuccess -> do
        newReq <- Y.decodeFileEither @Louter.ChatRequest filePath
        case newReq of
          Right newReq' -> return $ Just newReq'
          Left err -> do
            print err
            return Nothing
      ExitFailure _ -> return Nothing

editContentsWithEditor :: forall m. (MonadIO m, MonadFail m) => Contents -> m (Maybe Contents)
editContentsWithEditor contents = do
  liftIO $ withSystemTempFile "tempfile.yaml" $ \filePath fileHandle -> do
    hClose fileHandle
    BS.writeFile filePath $ Y.encodePretty Y.defConfig contents
    editor <- do
      lookupEnv "EDITOR" >>= \case
        Just editor' -> return editor'
        Nothing -> return "vim"
    code <- system (editor <> " " <> filePath)
    case code of
      ExitSuccess -> do
        newContents <- Y.decodeFileEither @Contents filePath
        case newContents of
          Right newContents' -> return $ Just newContents'
          Left err -> do
            print err
            return Nothing
      ExitFailure _ -> return Nothing

-- Now accepts an argument, for more commands.
runRepl' :: forall p. (PersistentBackend p) => [CommandSpec] -> Prompt (InputT IO) ()
runRepl' extraSpecs = do
  let allSpecs = extraSpecs ++ defaultCommands @p
  inLine <- lift $ getInputLine "% "
  case inLine of
    Nothing -> return ()
    Just input -> do
      if T.isPrefixOf ":" (T.pack input)
        then
        let result = parse (choice $ cmdParser <$> allSpecs) "stdin" (T.pack input)
        in case result of
             Right action -> action >> runRepl' @p extraSpecs
             Left err -> termOutput ("Unknown command: " <> T.pack (errorBundlePretty err) <> "\n") >> runRepl' @p extraSpecs
        else 
          callInput @p (T.pack input) >> runRepl' @p extraSpecs

runRepl :: forall p. (PersistentBackend p) => [ToolProxy] -> [CommandSpec] -> [CustomInstructionProxy] -> Text -> Louter.ChatRequest -> Contents -> IO ()
runRepl tools extensions customs sessionName defaultReq contents = do
  historyPath <- getDataDir >>= \dir ->
    lookupEnv "INTELLI_MONAD_DATA_DIR" >>= \case
      Just d  -> return $ d </> "history"
      Nothing -> return $ dir </> "history"
  runInputT
    ( Settings
        { complete = completeFilename,
          historyFile = Just historyPath,
          autoAddHistory = True
        }
    ) $
    do
      output <- getExternalPrint
      let callbackOut = \text -> output (T.unpack text)
          callbackIn = \prompt -> runInputT defaultSettings $
                                  fmap (fmap T.pack) (getInputLine $ T.unpack prompt)
      runPrompt @p tools customs sessionName defaultReq $ do
        prev <- get
        put $ prev { outputCallback = callbackOut
                   , inputCallback = callbackIn
                   , extraCommands = extensions
                   }
        push @p contents
        runRepl' @p extensions
