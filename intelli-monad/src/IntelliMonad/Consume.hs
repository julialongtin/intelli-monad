-- Entry point, for users of this library.
module IntelliMonad.Consume
  (
  CustomInstructionProxy(..)
  , Content(..)
  , CommandSpec(..)
  , CustomInstruction(..)
  , HasFunctionObject(..)
  , JSONSchema(..)
  , Message(..)
  , MonadTerminal(..)
  , Parser
  , Prompt
  , Schema(Object')
  , StatelessConf
  , Tool(..)
  , ToolProxy(..)
  , User(..)
  , callWithContents
  , showContents
  , defaultTools
  , defaultRequest
  , defaultUTCTime
  , fromModel
  , generate
  , initializePrompt
  , lexm
  , model
  , parseSessionName
  , readConfig
  , runPrompt
  , runPromptWithValidation
  , runRepl
  , toAeson
  , user
  )
where

import Prelude ()

import IntelliMonad.BaseTypes (
  CustomInstruction(customHeader, customFooter)
  , CustomInstructionProxy(CustomInstructionProxy)
  , CommandSpec(CommandSpec)
  , Content(Content, contentUser)
  , CustomInstruction
  , HasFunctionObject(getFieldDescription, getFunctionDescription, getFunctionName)
  , JSONSchema(schema)
  , Message(Message)
  , MonadTerminal(termInput, termOutput)
  , Prompt
  , Schema(Object')
  , Tool(Output, toolExec, toolHeader)
  , ToolProxy(ToolProxy)
  , User(Assistant, System, User)
  , toolExec
  , defaultUTCTime
  )

import IntelliMonad.Config (model, readConfig)

import IntelliMonad.Persist (StatelessConf)

import IntelliMonad.Parser (Parser)

import IntelliMonad.Prompt (
  callWithContents
  , generate
  , initializePrompt
  , runPrompt
  , runPromptWithValidation
  , showContents
  , user
  )

import IntelliMonad.Repl (lexm, parseSessionName, runRepl)

import IntelliMonad.Tools (defaultTools)

import IntelliMonad.Types (
  defaultRequest
  , fromModel
  , toAeson
  )
