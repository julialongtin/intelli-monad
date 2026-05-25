-- Entry point, for users of this library.
module IntelliMonad.Consume
  (
  CustomInstructionProxy(..)
  , Content(..)
  , CommandSpec(..)
  , CustomInstruction(..)
  , Example(..)
  , HasFunctionObject(..)
  , JSONSchema(..)
  , Message(..)
  , MonadTerminal(..)
  , Parser
  , Prompt
  , Schema(Boolean', Integer', Maybe', Object', String')
  , StatelessConf
  , Tool(..)
  , ToolProxy(..)
  , User(..)
  , callWithContents
  , showContents
  , defaultCommands
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
  , rejectUnknownKeys
  , runPrompt
  , runPromptWithValidation
  , runRepl
  , toAeson
  , user
  , warnUnknownKeys
  )
where

import Prelude ()

import IntelliMonad.BaseTypes (
  CustomInstruction(customHeader, customFooter)
  , CustomInstructionProxy(CustomInstructionProxy)
  , CommandSpec(CommandSpec)
  , Content(Content, contentUser)
  , CustomInstruction
  , Example(Example)
  , HasFunctionObject(getExamples, getFieldDescription, getFunctionDescription, getFunctionName)
  , JSONSchema(schema)
  , Message(Message)
  , MonadTerminal(termInput, termOutput)
  , Prompt
  , Schema(Boolean', Integer', Maybe', Object', String')
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

import IntelliMonad.Repl (defaultCommands, lexm, parseSessionName, runRepl)

import IntelliMonad.Tools (defaultTools)

import IntelliMonad.Tools.Utils (rejectUnknownKeys, warnUnknownKeys)

import IntelliMonad.Types (
  defaultRequest
  , fromModel
  , toAeson
  )
