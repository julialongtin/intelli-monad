{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This module is for helping the reflection, which maps the variables as known by the LLM when function calling, to the right structure components.

module IntelliMonad.Schema
  (
    -- * Generic JSON‑Schema builder that re‑uses HasFunctionObject
    mkSchemaFromHasFunctionObject
  ) where

import Data.Proxy (Proxy)

import IntelliMonad.BaseTypes (Schema(..), getFieldDescription)

import IntelliMonad.Tools (HasFunctionObject)

-- | Build an OpenAI‑compatible JSON schema for a tool that implements
--   `HasFunctionObject`.  You give it:
--     * a `Proxy a` – the type of the tool
--     * a list of field names (the *public* JSON keys you want to expose)
--     * a list of fields to be considered required
--     * a function that tells you which primitive schema to use for each key
--
--   The helper pulls the description from `getFieldDescription`.
mkSchemaFromHasFunctionObject
  :: forall a.
     (HasFunctionObject a)
  => Proxy a                -- ^ the tool type
  -> [(String, Schema)]     -- ^ (publicKey, primitiveSchema) for every field
  -> Schema                 -- ^ the full object schema
mkSchemaFromHasFunctionObject _ fields =
  Object' (map fieldWithDesc fields)
 where
   -- Attach description from `HasFunctionObject` and apply `Maybe'` when needed
   fieldWithDesc :: (String, Schema) -> (String, String, Schema)
   fieldWithDesc (k, prim) =
     let desc = getFieldDescription @a k                     -- description we already wrote
     in (k, desc, prim)
