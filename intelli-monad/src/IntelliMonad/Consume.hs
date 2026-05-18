-- Entry point, for users of this library.
module IntelliMonad.Consume
  (
  CustomInstructionProxy(..)
  , Content(..)
  , CustomInstruction(..)
  , HasFunctionObject(..)
  , JSONSchema(..)
  , Message(..)
  , MonadTerminal(..)
  , Schema(Object')
  , StatelessConf
  , Tool(..)
  , User(..)
  , callWithContents
  , showContents
  , defaultRequest
  , defaultUTCTime
  , fromModel
  , generate
  , initializePrompt
  , model
  , readConfig
  , runPrompt
  , runPromptWithValidation
  , toAeson
  , user
  )
where

import Prelude ()

import IntelliMonad.BaseTypes (
  CustomInstruction(customHeader, customFooter)
  , CustomInstructionProxy(CustomInstructionProxy)
  , Content(Content, contentUser)
  , CustomInstruction
  , HasFunctionObject(getFieldDescription, getFunctionDescription, getFunctionName)
  , JSONSchema(schema)
  , Message(Message)
  , MonadTerminal(termInput, termOutput)
  , Schema(Object')
  , Tool(Output, toolExec, toolHeader)
  , User(Assistant, System, User)
  , toolExec
  , defaultUTCTime
  )

import IntelliMonad.Config (model, readConfig)

import IntelliMonad.Persist (StatelessConf)

import IntelliMonad.Prompt (
  callWithContents
  , generate
  , initializePrompt
  , runPrompt
  , runPromptWithValidation
  , showContents
  , user
  )

import IntelliMonad.Types (
  defaultRequest
  , fromModel
  , toAeson
  )
