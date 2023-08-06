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
  } -- TODO req and response content types

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
  { reqPath :: T.Text
  , reqQueryRules :: Maybe QueryRules
  , reqTemplate :: T.Text
  }

mkRule :: RuleSpec -> Either String Rule
mkRule req = do
  template <- first show . Stache.compileTemplate ""
            $ reqTemplate req
  Right MkRule
    { pathRule = pathFromText $ reqPath req
    , queryRule = fromMaybe M.empty $ reqQueryRules req
    , response = template
    }

instance Aeson.FromJSON RuleSpec where
  parseJSON = Aeson.withObject "Rule Request" $ \o ->
    MkRuleSpec
    <$> o Aeson..: "path"
    <*> o Aeson..:? "query"
    <*> o Aeson..: "response"

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
