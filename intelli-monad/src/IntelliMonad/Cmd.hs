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

module IntelliMonad.Cmd
  (
    main
  )
where

import Prelude (IO, ($), (<>), (>>=), return)

import Data.Maybe (Maybe(Just, Nothing))

import Data.Text (Text, pack)

import Database.Persist.Sqlite (SqliteConf)

import Options.Applicative (argument, customExecParser, fullDesc, info, metavar, prefs, progDesc, showDefault, showHelpOnEmpty, str, value)

import System.Environment (lookupEnv)

import IntelliMonad.BaseTypes (PersistentBackend)

import IntelliMonad.Config (model, readConfig)

import IntelliMonad.Repl (defaultCommands, runRepl)

import IntelliMonad.Tools (defaultTools)

import IntelliMonad.Types (fromModel)


main :: IO ()
main = do
  -- Parse our config file. We only use the model, at this point.
  config <- readConfig

  -- Determine what model to use by default. Use what's in the config file by default, but override with an environment variable (OPENAI_MODEL).
  model <- do
    lookupEnv "OPENAI_MODEL" >>= \case
      Just model -> return $ pack model
      Nothing -> return config.model

  let cmdline = argument str (metavar "SESSION_NAME" <> value "default" <> showDefault)
      runCmd :: forall p. (PersistentBackend p) => Text -> IO ()
      runCmd sessionName = runRepl @p defaultTools (defaultCommands @p) [] sessionName (fromModel model) []

  sessionName <- customExecParser (prefs showHelpOnEmpty) (info (cmdline) (fullDesc <> progDesc "intelli-monad"))
  runCmd @SqliteConf sessionName
