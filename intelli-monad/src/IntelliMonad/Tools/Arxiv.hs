{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IntelliMonad.Tools.Arxiv
  (
    Arxiv
  )
where

import Control.Exception (catch)
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.ByteString (ByteString, toStrict)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Client (HttpException, newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS
import Network.HTTP.Simple (setRequestQueryString)
import Text.XML
import Text.XML.Cursor (Axis, Cursor, checkName, content, fromDocument, ($//), (&/))

import IntelliMonad.BaseTypes (Example(Example), HasFunctionObject(getExamples, getFieldDescription, getFunctionDescription, getFunctionName), JSONSchema, Tool(Output, toolExec))

data Arxiv = Arxiv
  { searchQuery :: Text,
    limit :: Maybe Int,
    offset :: Maybe Int
  }
  deriving (Eq, Show, Generic, JSONSchema)

-- JSON ↔ Haskell mapping that translates the public key "limit" to the private field 'maxResultCount'
instance A.FromJSON Arxiv where
  parseJSON = A.withObject "Arxiv" $ \obj ->
    Arxiv -- public name → internal field
      <$> obj A..: "searchQuery"
      <*> obj A..:? "limit"
      <*> obj A..:? "offset"

instance A.ToJSON Arxiv where
  toJSON v@(Arxiv {}) =
    A.object -- public name -> extracted value
    [ "path"    A..= (searchQuery v)
    , "limit"   A..= (limit v)
    , "offset"  A..= (offset v)
    ]

instance HasFunctionObject Arxiv where
  getFunctionName = "search_arxiv"
  getFunctionDescription = "Search Arxiv with a keyword"
  getFieldDescription "searchQuery" = "The keyword to search for on Arxiv: This keyword is used as a input of 'http://export.arxiv.org/api/query?search_query='. "
  getFieldDescription "limit" = "The maximum number of results to return. If not specified, the default is 10."
  getFieldDescription "offset" = "The start index of the results. If not specified, the default is 0."
  getFieldDescription _ = "⚠ Unknown field – check the schema."
  getExamples = Just [Example "search for 10 articles about LLM fine tuning."
                       $ A.object
                         [ "searchQuery" A..= ("LLM fine tuning" :: Text)
                         , "limit"       A..= (10 :: Int)
                         , "offset"       A..= (0 :: Int)
                         ]
                      ]

arxivSearch :: Arxiv -> IO ByteString
arxivSearch Arxiv {..} = do
  manager <- newManager tlsManagerSettings
  baseRequest <- parseRequest "https://export.arxiv.org/api/query"
  let request = setRequestQueryString
                [ ("search_query", Just $ T.encodeUtf8 searchQuery),
                  ("limit", Just $ fromMaybe "10" (BC.pack . show <$> limit)),
                  ("start", Just $ fromMaybe "0" (BC.pack . show <$> offset))
                ] baseRequest
  response <- httpLbs request manager `catch` \(e :: HttpException) -> error $ "Threw an HTTP exception: " <> show e <> "\n"
  return $ toStrict $ responseBody response

element' :: Text -> Axis
element' name = checkName (\n -> nameLocalName n == name)

queryArxiv :: Arxiv -> IO [ArxivEntry]
queryArxiv keyword = do
  jsonSource <- arxivSearch keyword :: IO ByteString
  return $ parseArxivXML jsonSource

data ArxivEntry = ArxivEntry
  { arxivId :: Text,
    published :: Text,
    title :: Text,
    summary :: Text
  }
  deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

headDef :: a -> [a] -> a
headDef d [] = d
headDef _ (x : _) = x

-- | Parser for an Arxiv Entry in XML
parseEntry :: Cursor -> Maybe ArxivEntry
parseEntry c =
  let arxivId = headDef "" $ c $// element' "id" &/ content
      published = headDef "" $ c $// element' "published" &/ content
      title = headDef "" $ c $// element' "title" &/ content
      summary = headDef "" $ c $// element' "summary" &/ content
   in Just $ ArxivEntry arxivId published title summary

-- | Parser for an Arxiv Result in XML
parseArxivResult :: Cursor -> [ArxivEntry]
parseArxivResult c = mapMaybe parseEntry (c $// element' "entry")

parseArxivXML :: ByteString -> [ArxivEntry]
parseArxivXML xml =
  case parseText def (TL.fromStrict $ T.decodeUtf8 xml) of
    Left _ -> []
    Right v -> parseArxivResult $ fromDocument v

instance Tool Arxiv where
  data Output Arxiv = ArxivOutput
    { papers :: [ArxivEntry]
    }
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = liftIO $ do
    papers <- queryArxiv args
    return $ ArxivOutput papers
