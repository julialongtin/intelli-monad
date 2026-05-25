{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

{-# LANGUAGE TypeFamilies #-}

module Tools.Man
  ( Man(..)                -- the request type (exposed for the proxy)
  ) where

import Prelude                (Eq, Int, Maybe(..), Show,
                               ($), (<$>), (<*>), (<>), (++),
                               show, null, pure)

import Control.Monad          (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson             ((.:), (.:?), (.=))
import qualified Data.Aeson   as A
import Data.Maybe (fromMaybe, maybe)

import Data.Text              (Text, pack, unpack)

import GHC.Generics           (Generic)

import Data.Proxy (Proxy(..))

import System.Process         (readProcessWithExitCode)
import System.Exit            (ExitCode(..))

import IntelliMonad.Schema    (mkSchemaFromHasFunctionObject)
import IntelliMonad.Consume   (Example(Example), HasFunctionObject(..), JSONSchema(..)
                              , Schema(Integer', Maybe', String')
                              , Tool(..)
                              , termOutput
                              , warnUnknownKeys)

--------------------------------------------------------------------------------
-- 1. Public request type
--------------------------------------------------------------------------------
data Man = Man
  { manPage    :: Text          -- ^ name of the page, e.g. “ls”
  , manSection :: Maybe Int    -- ^ optional section number (1‑9)
  , manWarnings :: Maybe [Text]   -- ^ populated automatically for unknown keys
  } deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 2. JSON ↔ Haskell conversion
--
-- The public key that the user sees is **“name”** (and not the internal field
-- “manPage”).  We also accept an optional “section”.  All other keys are
-- collected and reported as warnings – exactly the pattern used by the other
-- tools (`ListGitFiles`, `ReadGitFile` …).
--------------------------------------------------------------------------------
instance A.FromJSON Man where
  parseJSON = A.withObject "Man" $ \rawObj -> do
    let (obj, foundWarnings) = warnUnknownKeys ["name","section"] rawObj
    Man
      <$> obj .:  "name"                 -- public → internal
      <*> obj .:? "section"
      <*> pure (if null foundWarnings then Nothing else Just foundWarnings)

instance A.ToJSON Man where
  toJSON v = A.object
    [ "name"    .= (manPage v)
    , "section" .= (manSection v)
    ]

--------------------------------------------------------------------------------
-- 3. Describe the tool (schema, help‑text, examples)
--------------------------------------------------------------------------------
instance HasFunctionObject Man where
  getFunctionName        = "man"
  getFunctionDescription = "Show a Unix manual page.  The tool runs the system `man` \
                            \command and returns its stdout (or the error message)."
  getFieldDescription "name"    = "Name of the manual page (required).  Example: \"ls\"."
  getFieldDescription "section" = "Optional manual section (1‑9).  If omitted the \
                                   \system default is used."
  getFieldDescription _        = "⚠ Unknown field – check the schema."
  getExamples = Just
    [ Example "Show the synopsis for `ls` (any section)." $
        A.object [ "name"    .= ("ls"   :: Text) ]
    , Example "Show the POSIX description (section 1) for `printf`." $
        A.object [ "name"    .= ("printf" :: Text)
                 , "section" .= (1        :: Int) ]
    ]

instance JSONSchema Man where
  schema = mkSchemaFromHasFunctionObject (Proxy @Man)
    [ ("name",    String')
    , ("section", Maybe' Integer')
    ]

--------------------------------------------------------------------------------
-- 4. The actual implementation (`toolExec`)
--------------------------------------------------------------------------------
instance Tool Man where
  -- The output type is deliberately tiny – the REPL already prints everything
  -- that `termOutput` emits.
  data Output Man = ManOutput
    { manText             :: Text               -- ^ full page (or error)
    , manPageRequested    :: Text               -- ^ the page we asked for
    , manSectionRequested :: Maybe Int
    , warnings            :: Maybe [Text]       -- ^ the warning list from the parser
    } deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)

  toolExec args = do
    -- 1⃣ Emit any parser‑generated warnings first
    forM_ (fromMaybe [] (manWarnings args)) $ \w ->
      termOutput $ "[WARN] man – " <> w <> "\n"

    -- 2⃣ Build the command line
    let page    = unpack (manPage args)
        sectArg = maybe [] (\s -> ["-s", show s]) (manSection args)
        cmdArgs = sectArg ++ [page]

    -- 3⃣ Run `man`.  We capture both stdout and stderr because `man` prints
    --    the page on stdout and error messages on stderr.
    (exitCode, out, err) <- liftIO $ readProcessWithExitCode "man" cmdArgs ""

    let result = case exitCode of
          ExitSuccess -> out
          ExitFailure _ -> "⚠ man error:\n" <> err

    pure $ ManOutput
      { manText    = pack result
      , manPageRequested    = manPage args
      , manSectionRequested = manSection args
      , warnings   = manWarnings args
      }

--------------------------------------------------------------------------------
