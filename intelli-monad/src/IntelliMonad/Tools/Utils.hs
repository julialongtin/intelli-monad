{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE TypeFamilies #-}

module IntelliMonad.Tools.Utils
  (
    findToolCall
  , rejectUnknownKeys
  , tryToolExec
  , warnUnknownKeys
  )
where

import Control.Monad (forM, unless)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (FromJSON, Object, ToJSON, eitherDecode, encode)
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap (keys)

import qualified Data.Aeson.Types as Aeson (Parser)

import Data.ByteString (fromStrict, toStrict)

import Data.Maybe (catMaybes)

import Data.Proxy (Proxy(Proxy))

import Data.Text (Text, unpack)

import Data.Text.Encoding (encodeUtf8, decodeUtf8Lenient)

import Data.Time (getCurrentTime)

import IntelliMonad.BaseTypes (PersistentBackend, Content(Content), Contents, Message(Message, Image, ToolCall, ToolReturn), MonadTerminal, Output, Prompt, Tool(toolExec, toolFunctionName), ToolProxy(ToolProxy), User(Tool))

toolExec' ::
  forall t p m.
  (PersistentBackend p, MonadIO m, MonadFail m, MonadTerminal m, Tool t, FromJSON t, ToJSON (Output t)) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Prompt m (Maybe Content)
toolExec' sessionName id' name' args' = do
  if name' == toolFunctionName @t
    then case (eitherDecode (fromStrict (encodeUtf8 args')) :: Either String t) of
      Left _ -> return Nothing
      Right input -> do
        output <- toolExec @t @p @m input
        time <- liftIO getCurrentTime
        return $ Just $ (Content Tool (ToolReturn id' name' (decodeUtf8Lenient (toStrict (encode output)))) sessionName time)
    else return Nothing

(<||>) ::
  forall m.
  (MonadIO m, MonadFail m) =>
  (Text -> Text -> Text -> Text -> Prompt m (Maybe Content)) ->
  (Text -> Text -> Text -> Text -> Prompt m (Maybe Content)) ->
  Text ->
  Text ->
  Text ->
  Text ->
  Prompt m (Maybe Content)
(<||>) tool0 tool1 sessionName id' name' args' = do
  a <- tool0 sessionName id' name' args'
  case a of
    Just v -> return (Just v)
    Nothing -> tool1 sessionName id' name' args'

mergeToolCall :: forall p m. (PersistentBackend p, MonadIO m, MonadFail m, MonadTerminal m) => [ToolProxy] -> Text -> Text -> Text -> Text -> Prompt m (Maybe Content)
mergeToolCall [] _ _ _ _ = return Nothing
mergeToolCall (tool : tools') sessionName id' name' args' = do
  case tool of
    (ToolProxy (_ :: Proxy a)) -> (toolExec' @a @p <||> mergeToolCall @p tools') sessionName id' name' args'

hasToolCall :: Contents -> Bool
hasToolCall cs =
  let loop [] = False
      loop ((Content _ (ToolCall _ _ _) _ _) : _) = True
      loop (_ : cs') = loop cs'
   in loop cs

filterToolCall :: Contents -> Contents
filterToolCall cs =
  let loop [] = []
      loop (m@(Content _ (ToolCall _ _ _) _ _) : cs') = m : loop cs'
      loop (_ : cs') = loop cs'
   in loop cs

tryToolExec :: forall p m. (PersistentBackend p, MonadIO m, MonadFail m, MonadTerminal m) => [ToolProxy] -> Text -> Contents -> Prompt m Contents
tryToolExec tools sessionName contents = do
  cs <- forM (filterToolCall contents) $ \(Content _ (ToolCall id' name' args') _ _) -> do
    mergeToolCall @p tools sessionName id' name' args'
  return $ catMaybes cs

findToolCall :: ToolProxy -> Contents -> Maybe Content
findToolCall _ [] = Nothing
findToolCall t@(ToolProxy (Proxy :: Proxy a)) (c : cs) =
  case c of
    Content _ (Message _) _ _ -> findToolCall t cs
    Content _ (Image _ _) _ _ -> findToolCall t cs
    Content _ (ToolCall _ name' _) _ _ ->
      if name' == toolFunctionName @a
        then Just c
        else findToolCall t cs
    Content _ (ToolReturn _ _ _) _ _ -> findToolCall t cs

-- | Given a list of *allowed* field names, and the raw object,
--   either:
--     * succeed, returning the original object, or
--     * fail with a message listing every unknown key.
rejectUnknownKeys
  :: [Text]            -- ^ allowed keys (e.g. ["path","recurse","limit","offset"])
  -> Object          -- ^ the raw JSON object that was just received
  -> Aeson.Parser Object   -- ^ same object on success, or `fail` on error
rejectUnknownKeys allowed obj = do
  let present   = map toText (keys obj)
      unknown   = filter (`notElem` allowed) present
  unless (null unknown) $
    fail $ "Invalid argument(s): " ++ show (map unpack unknown) ++
           ". Accepted fields are: " ++ show (map unpack allowed)
  pure obj

-- | Validate that an object only contains keys from an allow‑list.
--   Instead of calling `fail` we return the original object **and**
--   a list of the unknown keys (empty if everything is fine).
warnUnknownKeys
    :: [Text]                -- ^ allowed keys
    -> Object              -- ^ raw JSON object
    -> (Object, [Text])    -- ^ (object, warnings)
warnUnknownKeys allowed obj =
    let present = map toText (keys obj)
        unknown = warning_message <$> filter (`notElem` allowed) present
        warning_message t = "Warning: Extra \"" <> t <> "\" attribute not interpreted by tool."
    in (obj, unknown)           -- the object is always returned


