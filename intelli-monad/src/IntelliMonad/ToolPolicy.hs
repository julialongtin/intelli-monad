-- Policy based tool handling. For the REPL.

module IntelliMonad.ToolPolicy
  (
    ToolEntry(toolPolicy),
    ToolRegistry(ToolRegistry, rawRegistry),
    addTool,
    checkPolicy,
    changeToolPolicy,
    defaultRegistry,
    getTools
  ) where

import IntelliMonad.BaseTypes (Content(Content), Context, HasFunctionObject(getFunctionDescription, getFunctionName), Message(ToolCall, ToolReturn), PersistentBackend, Prompt, PromptEnv(inputCallback, outputCallback), ToolProxy(ToolProxy), contextToolbox, Tool(toolFunctionName), User(Tool))

import IntelliMonad.ToolPolicy.Types (ToolEntry(ToolEntry, toolPolicy), ToolPolicy(Allow, Ask, Deny), ToolRegistry(ToolRegistry, rawRegistry))

import IntelliMonad.ToolPolicy.Utils (addTool, checkPolicy, changeToolPolicy, defaultRegistry, getTools)
