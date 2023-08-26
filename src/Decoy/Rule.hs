{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoy.Rule
  ( -- * Types
    RuleF(..)
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

-- | A rule that has been fully instantiated and can be added to a server instance.
--
-- @since 0.1.0.0
type Rule = RuleF [PathPart] [JP.JSONPathElement] Stache.Template

-- | A specification for a rule. Construct this using 'mkRuleSpec' and convert
-- to a 'Rule' using 'compileRule'.
--
-- @since 0.1.0.0
type RuleSpec = RuleF T.Text T.Text T.Text

-- | Base type for a rule, which can either be a 'Rule' or 'RuleSpec'.
--
-- @since 0.1.0.0
data RuleF urlPath jsonPath template = MkRule
  { request :: Request urlPath jsonPath
  , response :: Response template
  } deriving (Show, Eq, Functor)

-- | Request portion of a rule.
--
-- @since 0.1.0.0
data Request urlPath jsonPath = MkRequest
  { reqPath :: urlPath
  , reqQuery :: QueryRules
  , reqMethod :: Maybe T.Text
  , reqContentType :: Maybe T.Text
  , reqBodyRules :: [BodyRule jsonPath]
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Specifies which query arguments must be present for a rule to match and
-- optionally whether a specific value must be mapped to that argument.
--
-- @since 0.1.0.0
type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value

-- | The options used to match against paths in a JSON request body.
--
-- @since 0.1.0.0
data JsonPathOpts jsonPath = MkJsonPathOpts
  { jsonPath :: jsonPath
    -- ^ a JSON path which must match at least one element of the request body.
  , allMatch :: Bool
    -- ^ If True, all elements matched by the given JSON path must satisfy the rule.
    -- If False, at least one element must satisfy the rule.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Allows a rule to match against the contents of the request body.
--
-- @since 0.1.0.0
data BodyRule jsonPath = MkBodyRule
  { jsonPathOpts :: Maybe (JsonPathOpts jsonPath)
    -- ^ For JSON request bodies, specify a JSON path to where the regex should
    -- be matched
  , regex :: T.Text
    -- ^ A regular expression that must be matched against
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The response portion of a rule.
--
-- @since 0.1.0.0
data Response template = MkResponse
  { respBody :: ResponseBody template
  , respContentType :: Maybe T.Text
    -- ^ If specified, the request's @Accept@ header must contain this value.
  , respStatusCode :: Maybe Word
    -- ^ Specifies a status code for the response. @200@ is used if @Nothing@.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The response body that will be returned if a rule matches.
--
-- @since 0.1.0.0
data ResponseBody t
  = File FilePath -- ^ A static file
  | Template t -- ^ A mustache template
  | NoBody
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

-- | A helper for instantiating a 'RuleSpec' given a URL path and a
-- 'ResponseBody'.
--
-- __Examples:__
--
-- @
-- mkRuleSpec "users/:userId/delete" (Template "User {{path.userId}} deleted")
-- mkRuleSpec "items" (File "./items.json")
-- @
--
-- @since 0.1.0.0
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

-- | Set the URL path of a 'RuleSpec'.
--
-- @since 0.1.0.0
setUrlPath :: T.Text -> RuleSpec -> RuleSpec
setUrlPath p r = r { request = (request r) { reqPath = p } }

-- | Add a rule for query string arguments.
--
-- __Examples:__
--
-- @
-- addQueryRule "key" Nothing $ mkRuleSpec "some/path" (Template "body")
-- @
-- creates a rule that matches a request to @some/path?key@
-- @
-- addQueryRule "key" (Just "val") $ mkRuleSpec "some/path" (Template "body")
-- @
-- creates a rule that matches a request to @some/path?key=val@
--
-- @since 0.1.0.0
addQueryRule
  :: T.Text -- ^ The query argument key
  -> Maybe T.Text -- ^ A value that must be assigned to the query argument
  -> RuleF a b c
  -> RuleF a b c
addQueryRule k v r = r { request = (request r) { reqQuery = M.insert k v $ reqQuery (request r) } }

-- | Replace all query rules within a rule.
--
-- @since 0.1.0.0
setQueryRules :: QueryRules -> RuleF a b c -> RuleF a b c
setQueryRules q r = r { request = (request r) { reqQuery = q } }

-- | Specifies the request method that must be used for a rule to match.
--
-- __Example:__
--
-- @
-- setReqMethod "POST" $ mkRuleSpec "some/path" (Template "body")
-- @
--
-- @since 0.1.0.0
setReqMethod :: T.Text -> RuleF a b c -> RuleF a b c
setReqMethod m r = r { request = (request r) { reqMethod = Just m } }

-- | Set a content type that must be contained in the @Content-Type@ header
-- of a request for the rule to match.
--
-- __Example:__
--
-- @
-- setReqContentType "json" $ mkRuleSpec "some/path" (Template "body")
-- @
--
-- @since 0.1.0.0
setReqContentType :: T.Text -> RuleF a b c -> RuleF a b c
setReqContentType c r = r { request = (request r) { reqContentType = Just c } }

-- | Add a body rule which must match against the request body for the endpoint
-- rule to match.
--
-- __Examples:__
--
-- @
-- addBodyRule 'MkBodyRule' { jsonPathOpts = Nothing, regex = "^a regex.*" } someRule
-- @
--
-- @
-- addBodyRule 'MkBodyRule'
--   { jsonPathOpts = MkJsonPathOpts
--     { jsonPath = "$.path.to.field"
--     , allMatch = True
--     }
--   , regex = "^a regex.*"
--   } someRule
-- @
--
-- @since 0.1.0.0
addBodyRule :: BodyRule T.Text -> RuleSpec -> RuleSpec
addBodyRule b r = r { request = (request r) { reqBodyRules = b : reqBodyRules (request r) } }

-- | Replace all request body rules in an endpoint rule.
--
-- @since 0.1.0.0
setBodyRules :: [BodyRule T.Text] -> RuleSpec -> RuleSpec
setBodyRules b r = r { request = (request r) { reqBodyRules = b } }

-- | Replace the response body of a rule.
--
-- @since 0.1.0.0
setBody :: ResponseBody T.Text -> RuleSpec -> RuleSpec
setBody b r = r { response = (response r) { respBody = b } }

-- | Set a content type that must be contained in the @Accept@ header
-- of a request for the rule to match.
--
-- __Example:__
--
-- @
-- setRespContentType "text/plain" $ mkRuleSpec "some/path" (Template "body")
-- @
--
-- @since 0.1.0.0
setRespContentType :: T.Text -> RuleF a b c -> RuleF a b c
setRespContentType c r = r { response = (response r) { respContentType = Just c } }

-- | Set the status code of the response returned if the given rule matches.
--
-- __Example:__
--
-- @
-- setStatusCode 500 $ mkRuleSpec "some/path" (Template "body")
-- @
--
-- @since 0.1.0.0
setStatusCode :: Word -> RuleF a b c -> RuleF a b c
setStatusCode c r = r { response = (response r) { respStatusCode = Just c } }

-- | Attempts to convert a 'RuleSpec' to a 'Rule'. It will return a @Left@ if
-- the rule is invalid in some way.
--
-- @since 0.1.0.0
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
              "file" -> maybe (fail "no body") (pure . File) val
              "template" -> maybe (fail "no body") (pure . Template . T.pack) val
              "no_body" -> pure NoBody
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
                  File{} -> "file"
                  NoBody -> "no_body"
                )
    , "body" .= (case respBody r of
                  Template t -> Just t
                  File f -> Just $ T.pack f
                  NoBody -> Nothing
                )
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
