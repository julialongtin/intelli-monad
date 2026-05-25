{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Tools.Hello where

import Prelude ( Eq, Show, ($), (<>), return)

import GHC.Generics (Generic)

import Data.Maybe (Maybe(Just))

import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON, (.=), object)

import IntelliMonad.Consume
  ( 
    Example(Example)
  , HasFunctionObject(..)
  , JSONSchema(..)
  , Tool(..)
  , getExamples
  )

-- ── Hello World tool ─────────────────────────────────────────────────────────

data Hello = Hello { name :: Text }
  deriving (Eq, Show, Generic, JSONSchema, FromJSON, ToJSON)

instance HasFunctionObject Hello where
  getFunctionName        = "hello"
  getFunctionDescription = "Say hello to someone by name"
  getFieldDescription "name" = "The name to greet. For example: \"Juri\""
  getFieldDescription _      = "⚠ Unknown field – check the schema."
  getExamples = Just $ [ Example "say Hello to World" $ object [ "name" .= ("World" :: Text)] ]

instance Tool Hello where
  data Output Hello = HelloOutput { greeting :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
  toolExec args = return $ HelloOutput $ "Hello, " <> args.name <> "!"

