{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoy.Rule
  ( RuleF(..)
  , Rule
  , RuleSpec
  , Response(..)
  , ResponseBody(..)
  , Request(..)
  , JsonPathOpts(..)
  , BodyRule(..)
  , QueryRules
  , PathPart(..)
  -- * Instantiating rules
  , mkRuleSpec
  , compileRule
  -- * Rule modifiers
  , setUrlPath
  , addQueryRule
  , setQueryRules
  , setReqMethod
  , setReqContentType
  , addBodyRule
  , setBodyRules
  , setBody
  , setRespContentType
  , setStatusCode
  ) where

import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.JSONPath as JP
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Mustache as Stache

type Rule = RuleF [PathPart] [JP.JSONPathElement] Stache.Template

data RuleF urlPath jsonPath template = MkRule
  { request :: Request urlPath jsonPath
  , response :: Response template
  } deriving (Show, Eq, Functor)

data Request urlPath jsonPath = MkRequest
  { reqPath :: urlPath
  , reqQuery :: QueryRules
  , reqMethod :: Maybe T.Text
  , reqContentType :: Maybe T.Text
  , reqBodyRules :: [BodyRule jsonPath]
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data JsonPathOpts jsonPath = MkJsonPathOpts
  { jsonPath :: jsonPath
    -- ^ a JSON path which must match at least one element of the request body.
  , allMatch :: Bool
    -- ^ If True, all elements matched by the given JSON path must satisfy the rule.
    -- If False, at least one element must satisfy the rule.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data BodyRule jsonPath = MkBodyRule
  { jsonPathOpts :: Maybe (JsonPathOpts jsonPath)
    -- ^ For JSON request bodies, specify a JSON path to where the regex should
    -- be matched
  , regex :: T.Text
    -- ^ A regular expression that must be matched against
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data Response template = MkResponse
  { respBody :: ResponseBody template
  , respContentType :: Maybe T.Text
  , respStatusCode :: Maybe Word
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data ResponseBody t
  = File FilePath
  | Template t -- Stach.Template
  deriving (Show, Eq, Functor, Foldable, Traversable)

data PathPart
  = Static T.Text
  | PathParam T.Text
  deriving (Show, Eq)

pathFromText :: T.Text -> [PathPart]
pathFromText txt = parsePart <$> T.split (== '/') txt
  where
    parsePart p =
      case T.stripPrefix ":" p of
        Nothing -> Static p
        Just r -> PathParam r

-- pathToText :: [PathPart] -> T.Text
-- pathToText = T.intercalate "/" . map partToText
--   where
--     partToText = \case
--       Static t -> t
--       PathParam t -> ":" <> t

type RuleSpec = RuleF T.Text T.Text T.Text

mkRuleSpec :: T.Text -> ResponseBody T.Text -> RuleSpec
mkRuleSpec urlPath body =
  MkRule
    { request = MkRequest
        { reqPath = urlPath
        , reqQuery = mempty
        , reqMethod = Nothing
        , reqContentType = Nothing
        , reqBodyRules = []
        }
    , response = MkResponse
        { respBody = body
        , respContentType = Nothing
        , respStatusCode = Nothing
        }
    }

setUrlPath :: T.Text -> RuleSpec -> RuleSpec
setUrlPath p r = r { request = (request r) { reqPath = p } }

addQueryRule :: T.Text -> Maybe T.Text -> RuleF a b c -> RuleF a b c
addQueryRule k v r = r { request = (request r) { reqQuery = M.insert k v $ reqQuery (request r) } }

setQueryRules :: QueryRules -> RuleF a b c -> RuleF a b c
setQueryRules q r = r { request = (request r) { reqQuery = q } }

setReqMethod :: T.Text -> RuleF a b c -> RuleF a b c
setReqMethod m r = r { request = (request r) { reqMethod = Just m } }

setReqContentType :: T.Text -> RuleF a b c -> RuleF a b c
setReqContentType c r = r { request = (request r) { reqContentType = Just c } }

addBodyRule :: BodyRule T.Text -> RuleSpec -> RuleSpec
addBodyRule b r = r { request = (request r) { reqBodyRules = b : reqBodyRules (request r) } }

setBodyRules :: [BodyRule T.Text] -> RuleSpec -> RuleSpec
setBodyRules b r = r { request = (request r) { reqBodyRules = b } }

setBody :: ResponseBody T.Text -> RuleSpec -> RuleSpec
setBody b r = r { response = (response r) { respBody = b } }

setRespContentType :: T.Text -> RuleF a b c -> RuleF a b c
setRespContentType c r = r { response = (response r) { respContentType = Just c } }

setStatusCode :: Word -> RuleF a b c -> RuleF a b c
setStatusCode c r = r { response = (response r) { respStatusCode = Just c } }

compileRule :: RuleSpec -> Either String Rule
compileRule rs = do
  body <- traverse (first show . Stache.compileTemplate "") $ response rs
  req <- traverse (first P.errorBundlePretty . P.runParser (JP.jsonPath P.eof) "")
       $ request rs

  Right MkRule
    { response = body
    , request = req { reqPath = pathFromText $ reqPath req }
    }

instance FromJSON RuleSpec where
  parseJSON = withObject "rule" $ \o ->
    MkRule
    <$> o .: "request"
    <*> o .: "response"

instance FromJSON (Response T.Text) where
  parseJSON = withObject "response" $ \o -> do
    ty <- o .: "type"
    val <- o .: "body"
    MkResponse
      <$> ( case ty :: String of
              "file" -> pure $ File val
              "template" -> pure . Template $ T.pack val
              _ -> fail $ "invalid response body type: " <> ty
          )
      <*> o .:? "contentType"
      <*> o .:? "statusCode"

instance FromJSON (Request T.Text T.Text) where
  parseJSON = withObject "request" $ \o ->
    MkRequest
    <$> o .: "path"
    <*> o .:? "query" .!= mempty
    <*> o .:? "method"
    <*> o .:? "contentType"
    <*> o .:? "bodyRules" .!= []

instance FromJSON (JsonPathOpts T.Text) where
  parseJSON = withObject "JSON path opts" $ \o ->
    MkJsonPathOpts
    <$> o .: "jsonPath"
    <*> o .:? "allMatch" .!= True

instance FromJSON (BodyRule T.Text) where
  parseJSON = withObject "body rule" $ \o ->
    MkBodyRule
    <$> o .:? "jsonPathOpts"
    <*> o .: "regex"

instance ToJSON RuleSpec where
  toJSON rs = object
    [ "request" .= request rs
    , "response" .= response rs
    ]

instance ToJSON (Response T.Text) where
  toJSON r = object
    [ "type" .= (case respBody r of
                  Template{} -> ("template" :: T.Text)
                  File{} -> "file")
    , "body" .= (case respBody r of
                  Template t -> t
                  File f -> T.pack f)
    , "contentType" .= respContentType r
    , "statusCode" .= respStatusCode r
    ]

instance ToJSON (Request T.Text T.Text) where
  toJSON r = object
    [ "path" .= reqPath r
    , "query" .= reqQuery r
    , "method" .= reqMethod r
    , "contentType" .= reqContentType r
    , "bodyRules" .= reqBodyRules r
    ]

instance ToJSON (BodyRule T.Text) where
  toJSON r = object
    [ "jsonPathOpts" .= jsonPathOpts r
    , "regex" .= regex r
    ]

instance ToJSON (JsonPathOpts T.Text) where
  toJSON o = object
    [ "jsonPath" .= jsonPath o
    , "allMatch" .= allMatch o
    ]

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
