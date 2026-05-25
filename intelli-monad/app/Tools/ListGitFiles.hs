{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}


-- for MonadTerminal
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE InstanceSigs #-}

module Tools.ListGitFiles
  where

import Prelude (Bool(False, True), Eq, Int, Show, String, (.), (+), (<), ($), (<>), (<$>), (<*>), drop, filter, length, max, not, null, pure, take)

import Control.Monad (forM_)

import Control.Monad.IO.Class (liftIO)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:?), (.=), withObject, object)

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Data.Proxy (Proxy(Proxy))

import Data.Text (Text, isInfixOf, pack, splitOn, strip, unpack) 

import qualified Data.Text as DT (drop, length, null)

import GHC.Generics (Generic)

import System.Directory (getCurrentDirectory)

import System.Exit (ExitCode(ExitSuccess, ExitFailure))

import System.Process (readProcessWithExitCode)

import IntelliMonad.Consume
  ( Example(Example)
  , HasFunctionObject(..)
  , JSONSchema(..)
  , MonadTerminal(..)
  , Schema(Boolean', Integer', Maybe', String')
  , Tool(..)
  , getExamples
  , warnUnknownKeys
  )

import IntelliMonad.Schema
  (
    mkSchemaFromHasFunctionObject
  )

-- ── Git ls file tool ─────────────────────────────────────────────────────────

data ListGitFiles = ListGitFiles
  { gitSubDirPath :: Maybe Text
  , recurse :: Maybe Bool
  , limit :: Maybe Int
  , offset :: Maybe Int
  , listWarnings :: Maybe [Text]
  }
  deriving (Eq, Show, Generic)

-- JSON ↔ Haskell mapping that translates the public key "path" to the private field 'gitSubDirPath'
instance FromJSON ListGitFiles where
  parseJSON = withObject "ListGitFiles" $ \rawObj -> do
    -- First check for stray keys:
    let (obj, foundWarnings) = warnUnknownKeys ["path", "recurse", "limit", "offset"] rawObj -- <-- whitelist
    ListGitFiles -- public names here
      <$> obj .:? "path"
      <*> obj .:? "recurse"
      <*> obj .:? "limit"
      <*> obj .:? "offset"
      <*> pure (if null foundWarnings then Nothing else Just foundWarnings)

instance ToJSON ListGitFiles where
  toJSON v@(ListGitFiles {}) = object -- public names -> accessor
    [ "path"    .= (gitSubDirPath v)
    , "recurse" .= (recurse v)
    , "limit"   .= (limit v)
    , "offset"  .= (offset v)
    ]

instance HasFunctionObject ListGitFiles where
  getFunctionName        = "list_git_files"
  getFunctionDescription = "List files in the user's git repository (working tree, not HEAD)"
  getFieldDescription "path" = "Optional path relative to the git repository root to list files within. If omitted, lists files from the repository root. Only lists paths within the user's git repository. For example: \"programs/\""
  getFieldDescription "recurse" = "Optional flag indicating whether to recursively list files under the given path, or only list files contained within the directory given in path. If False(the default), only lists immediate children."
  getFieldDescription "limit" = "Optional maximum number of file listings to return. If omitted, returns all matching file listings. Use this to avoid overwhelmingly large responses."
  getFieldDescription "offset" = "Zero‑based index of the first file to return. If ommitted, starts with the first file listing. Used together with `limit` for paging."
  getFieldDescription _ = "⚠ Unknown field – check the schema."
{-  getExample = Just $ object
    []
  getExample = Just $ object
    [ 
-}
  getExamples = Just $ [
    Example "Get a listing of all of the files in the root directory of the git repository. Does not recurse." $ object []
    , Example " Get a listing of the files immediately in the 'docs' directory. Does not recurse."
             $ object
                [ "path"    .= ("docs/" :: Text)
                , "recurse" .= False
                ]
    , Example "Get a listing of ten of the files under src/, recursively starting with the 21st file, and ending with the 30th."
             $ object
                [ "path"    .= ("src/" :: Text)
                , "recurse" .= True
                , "limit"   .= (10 :: Int)
                , "offset"  .= (20 :: Int)
                ]
    ]
-- And now we define the Types and names, as the models see them.
instance JSONSchema ListGitFiles where
  schema = mkSchemaFromHasFunctionObject (Proxy @ListGitFiles)
            [
              ("path", Maybe' String')
            , ("recurse", Maybe' Boolean')
            , ("limit", Maybe' Integer')
            , ("offset", Maybe' Integer')
            ]

instance Tool ListGitFiles where
  data Output ListGitFiles = ListGitFilesOutput
    { files         :: [Text]
    , repoRoot      :: Text
    , listedPath    :: Text
    , totalMatched  :: Int
    , resultLimited :: Bool
    , nextOffset    :: Maybe Int
    , listWarned    :: Maybe [Text]
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)
  toolExec args = do
    forM_ (fromMaybe [] args.listWarnings) $ \w ->
      termOutput $ pack $ "[WARN] list_git_files – unknown key: " <> unpack w <> "\n"
    liftIO $ do
      cwd <- getCurrentDirectory
      let
        requestedPath :: Text
        requestedPath = fromMaybe "" args.gitSubDirPath
        gitCmd1, gitCmd2 :: [String]
        gitCmd1 = ["rev-parse", "--show-toplevel"]
        gitCmd2 = ("ls-files": "-z" : if DT.null requestedPath then [] else ["--", unpack requestedPath])
      res1 <- readProcessWithExitCode "git" ("-C" : cwd : gitCmd1) ""
      case res1 of
        (ExitFailure _, _, _) -> pure $ emptyOutput (pack cwd) requestedPath
        (ExitSuccess, gitRootRaw, _) -> do
          let
            gitRoot = unpack . strip . pack $ gitRootRaw
          -- zero separated list.
          res2 <- readProcessWithExitCode "git" ("-C" : gitRoot : gitCmd2) ""
          let
            filesAll = case res2 of
                         -- FIXME: should we do more with failure here?
                         (ExitFailure _, _, _) -> []
                         (ExitSuccess, zeroFilesRaw, _) -> filter (not . DT.null) $ splitOn "\0" $ pack zeroFilesRaw
            filesRecursed = case fromMaybe False args.recurse of
                              -- Return recursive ls.
                              True -> filesAll
                              -- Strip out everything but files immediately in the directory in question.
                              False -> filterDirectChildren requestedPath filesAll
            total = length filesRecursed
            requestedOffset = max 0 $ fromMaybe 0 args.offset
            filesDropped = drop requestedOffset filesRecursed
            (filesLimited, limited) = case args.limit of
                                        Nothing -> (filesDropped, False) -- No limits.
                                        Just n -> (take (max 0 n) $ drop requestedOffset filesRecursed, requestedOffset + n < total)
            in
            pure $ ListGitFilesOutput { files = filesLimited
                                      , repoRoot = pack gitRoot
                                      , listedPath = requestedPath
                                      , totalMatched = total
                                      , resultLimited = limited
                                      , nextOffset = if limited then Just (requestedOffset + length filesLimited) else Nothing
                                      , listWarned = args.listWarnings
                                      }

-- Generate an empty output result.
emptyOutput :: Text -> Text -> Output ListGitFiles
emptyOutput rootPath requestedPath =
  ListGitFilesOutput
  { files = []
  , repoRoot = rootPath
  , listedPath = requestedPath
  , totalMatched = 0
  , resultLimited = False
  , nextOffset = Nothing
  , listWarned = Nothing
  }

-- Remove results that would be normally in a recursive result, aka, de-recursivize it.
filterDirectChildren :: Text -> [Text] -> [Text]
filterDirectChildren requestedPath =
  filter $ \fileName ->
             let
               prefix = if DT.null requestedPath then "" else requestedPath <> "/"
               rest = DT.drop (DT.length prefix) fileName
             in
               not $ "/" `isInfixOf` rest

