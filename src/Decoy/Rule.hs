{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Decoy.Rule
  ( RuleF(..)
  , Rule
  , RuleSpec
  , Response(..)
  , ResponseBody(..)
  , Request(..)
  , compileRule
  , QueryRules
  , PathPart(..)
  ) where

import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Text.Mustache as Stache

type Rule = RuleF [PathPart] Stache.Template

data RuleF path template = MkRule
  { request :: Request path
  , response :: Response template
  } deriving (Eq, Functor)

data Request path = MkRequest
  { reqPath :: path
  , reqQuery :: QueryRules
  , reqMethod :: Maybe T.Text
  , reqContentType :: Maybe T.Text
  } deriving (Eq, Functor)

data Response template = MkResponse
  { respBody :: ResponseBody template
  , respContentType :: Maybe T.Text
  } deriving (Eq, Functor, Foldable, Traversable)

data ResponseBody t
  = File FilePath
  | Template t -- Stach.Template
  deriving (Eq, Functor, Foldable, Traversable)

data PathPart
  = Static T.Text
  | PathParam T.Text
  deriving Eq

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

type RuleSpec = RuleF T.Text T.Text

compileRule :: RuleSpec -> Either String Rule
compileRule rs = do
  body <- traverse (first show . Stache.compileTemplate "") $ response rs

  Right MkRule
    { response = body
    , request = pathFromText <$> request rs
    }

instance FromJSON RuleSpec where
  parseJSON = withObject "Rule" $ \o ->
    MkRule
    <$> o .: "request"
    <*> o .: "response"

instance FromJSON (Response T.Text) where
  parseJSON = withObject "Response" $ \o -> do
    ty <- o .: "type"
    val <- o .: "body"
    MkResponse
      <$> ( case ty :: String of
              "file" -> pure $ File val
              "template" -> pure . Template $ T.pack val
              _ -> fail $ "invalid response body type: " <> ty
          )
      <*> o .:? "contentType"

instance FromJSON (Request T.Text) where
  parseJSON = withObject "Request" $ \o ->
    MkRequest
    <$> o .: "path"
    <*> o .:? "query" .!= mempty
    <*> o .:? "method"
    <*> o .:? "contentType"

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
    ]

instance ToJSON (Request T.Text) where
  toJSON r = object
    [ "path" .= reqPath r
    , "query" .= reqQuery r
    , "method" .= reqMethod r
    , "contentType" .= reqContentType r
    ]

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
