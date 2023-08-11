{-# LANGUAGE OverloadedStrings #-}
module Decoy.Rule
  ( Rule(..)
  , RuleSpec(..)
  , mkRule
  , QueryRules
  , PathPart(..)
  ) where

import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Text.Mustache as Stache

data Rule = MkRule
  { pathRule :: [PathPart]
  , queryRule :: QueryRules
  , response :: Stache.Template
  , requestContentType :: Maybe T.Text
  , responseContentType :: Maybe T.Text
  , method :: Maybe T.Text
  }

data PathPart
  = Static T.Text
  | PathParam T.Text

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

data RuleSpec = MkRuleSpec
  { rsPath :: T.Text
  , rsQueryRules :: Maybe QueryRules
  , rsTemplate :: T.Text
  , rsRequestContentType :: Maybe T.Text
  , rsResponseContentType :: Maybe T.Text
  , rsMethod :: Maybe T.Text
  }

mkRule :: RuleSpec -> Either String Rule
mkRule req = do
  template <- first show . Stache.compileTemplate ""
            $ rsTemplate req
  Right MkRule
    { pathRule = pathFromText $ rsPath req
    , queryRule = fromMaybe M.empty $ rsQueryRules req
    , response = template
    , requestContentType = rsRequestContentType req
    , responseContentType = rsResponseContentType req
    , method = rsMethod req
    }

instance Aeson.FromJSON RuleSpec where
  parseJSON = Aeson.withObject "Rule Request" $ \o ->
    MkRuleSpec
    <$> o Aeson..: "path"
    <*> o Aeson..:? "query"
    <*> o Aeson..: "responseTemplate"
    <*> o Aeson..:? "requestContentType"
    <*> o Aeson..:? "responseContentType"
    <*> o Aeson..:? "method"

instance Aeson.ToJSON RuleSpec where
  toJSON r = Aeson.object
    [ "path" Aeson..= rsPath r
    , "query" Aeson..= rsQueryRules r
    , "responseTemplate" Aeson..= rsTemplate r
    , "requestContentType" Aeson..= rsRequestContentType r
    , "responseContentType" Aeson..= rsResponseContentType r
    , "method" Aeson..= rsMethod r
    ]

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
