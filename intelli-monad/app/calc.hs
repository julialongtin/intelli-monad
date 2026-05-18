{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Prelude (Double, Eq, Int, IO, Show, String, ($), (<$>), print, return)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (Maybe(Just))
import Data.Text (Text)
import Data.Text.IO (getLine, putStr)
import GHC.Generics (Generic)

import IntelliMonad.Consume (Content(Content), HasFunctionObject(getFieldDescription, getFunctionDescription, getFunctionName), MonadTerminal(termInput, termOutput), StatelessConf, Tool(Output, toolExec, toolHeader), JSONSchema, Message(Message), User(System), defaultUTCTime, fromModel, generate, model, readConfig, runPromptWithValidation, user)

data ValidateNumber = ValidateNumber
  { number :: Double
  }
  deriving (Eq, Show, Generic, JSONSchema, FromJSON, ToJSON)

instance HasFunctionObject ValidateNumber where
  getFunctionName = "output_number"
  getFunctionDescription = "validate input number"
  getFieldDescription "number" = "A number that system outputs."

instance Tool ValidateNumber where
  data Output ValidateNumber = ValidateNumberOutput
    { code :: Int,
      stdout :: String,
      stderr :: String
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
  toolExec _ = return $ ValidateNumberOutput 0 "" ""
  toolHeader = [(Content System (Message "Calcurate user input, then output just the number. Then call 'output_number' function.") "" defaultUTCTime)]


data Number = Number
  { number :: Double
  }
  deriving (Eq, Show, Generic, JSONSchema, FromJSON, ToJSON)

instance HasFunctionObject Number where
  getFunctionName = "number"
  getFunctionDescription = "validate input"
  getFieldDescription "number" = "A number"

instance Tool Number where
  data Output Number = NumberOutput
    { code :: Int,
      stdout :: String,
      stderr :: String
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
  toolExec _ = return $ NumberOutput 0 "" ""

data Input = Input
  { formula :: Text
  }
  deriving (Eq, Show, Generic, JSONSchema, FromJSON, ToJSON)

instance HasFunctionObject Input where
  getFunctionName = "formula"
  getFunctionDescription = "Describe a formula"
  getFieldDescription "formula" = "A formula"

instance MonadTerminal IO where
  termOutput  = putStr
  termInput _ = Just <$> getLine

main :: IO ()
main = do
  config <- readConfig
  v <- runPromptWithValidation @ValidateNumber @StatelessConf [] [] "default" (fromModel config.model) "2+3+3+sin(3)"
  print (v :: Maybe ValidateNumber)

  v <- generate [user "Calcurate a formula"] (Input "1+3")
  print (v :: Maybe Number)
