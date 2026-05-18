{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module ArxivSpec (spec) where

import Prelude (IO, Maybe(Just), (.), ($), (>), (<=), (<$>), length, not, null, show)
import Test.Hspec

import Data.Text.IO (getLine, putStr)

import IntelliMonad.Tools.Arxiv

import IntelliMonad.Consume (JSONSchema(schema), MonadTerminal(termOutput, termInput), Schema(Object'), StatelessConf, defaultRequest, runPrompt, toolExec)

-- short circuit MonadTerminal to the Text library.
instance MonadTerminal IO where
  termOutput  = putStr
  termInput _ = Just <$> getLine

spec :: Spec
spec = do
  describe "Arxiv Tool" $ do
    describe "Schema Generation" $ do
      it "generates correct JSON schema" $ do
        let arxivSchema = schema @Arxiv
        case arxivSchema of
          Object' fields -> do
            length fields `shouldBe` 3
            -- Check that all expected fields are present
            let fieldNames = (\(name, _, _) -> name) <$> fields
            fieldNames `shouldContain` ["searchQuery"]
            fieldNames `shouldContain` ["maxResults"]
            fieldNames `shouldContain` ["start"]
          _ -> expectationFailure "Expected Object' schema"

    describe "Real API Request" $ do
      it "makes successful request to Arxiv API and retrieves papers" $ do
        -- Create Arxiv query
        let arxivQuery = Arxiv
              { searchQuery = "quantum computing"
              , maxResults = Just 3
              , start = Just 0
              }

        -- Run toolExec in the Prompt monad using runPrompt like calc.hs does
        result <- runPrompt @StatelessConf [] [] "test-session" (defaultRequest) $ do
          toolExec @Arxiv @StatelessConf arxivQuery

        -- Verify results
        length (papers result) `shouldSatisfy` (> 0)
        length (papers result) `shouldSatisfy` (<= 3)

        -- Verify first paper has required fields
        case papers result of
          (firstPaper:_) -> do
            arxivId firstPaper `shouldSatisfy` (not . null . show)
            title firstPaper `shouldSatisfy` (not . null . show)
            summary firstPaper `shouldSatisfy` (not . null . show)
            published firstPaper `shouldSatisfy` (not . null . show)
          [] -> expectationFailure "No papers returned"

      it "handles different search queries" $ do
        let arxivQuery = Arxiv
              { searchQuery = "neural networks"
              , maxResults = Just 2
              , start = Just 0
              }

        -- Run toolExec using runPrompt
        result <- runPrompt @StatelessConf [] [] "test-session" (defaultRequest) $ do
          toolExec @Arxiv @StatelessConf arxivQuery

        length (papers result) `shouldSatisfy` (> 0)
        length (papers result) `shouldSatisfy` (<= 2)
