{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
module Decoy.Modifier
  ( -- * Types
    ModifierF(..)
  , ModifierSpec
  , Modifier
  , ModifierId(..)
  , ModifierWithId
  , Matcher(..)
  , MatcherSpec
  -- * Instantiating modifiers
  , mkModifierSpec
  , compileModifier
  , initModifierId
  ) where

import           Data.Aeson
import qualified Data.Aeson.Patch as AP
import qualified Data.Text as T

import           Decoy.Rule

-- | A specification for a modifier. Construct this using 'mkModifierSpec' and convert
-- to a 'Modifier' using 'compileModifier'.
--
-- Modifiers are used to apply a JSON patch to the response for requests that
-- match a rule. You can either define a request matcher that is specific to
-- the modifier or use the ID of an existing rule. For example, if you want to
-- add a key to a response for a specific test scenario, you could do that by
-- adding a modifier in that test.
--
-- The JSON patch functionality comes from the `aeson-diff` package.
--
-- @since 0.1.0.0
type ModifierSpec = ModifierF NoId RequestSpec

type Modifier = ModifierF NoId Request
type ModifierWithId = ModifierF ModifierId Request

newtype ModifierId = MkModifierId Int
  deriving (Enum, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, Show)

initModifierId :: ModifierId
initModifierId = MkModifierId 1

data ModifierF id req = MkModifier
  { matcher :: Matcher req
  , jsonPatch :: AP.Patch
  , modifierId :: id
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A helper for instantiating a 'ModifierSpec'.
--
-- __Examples:__
--
-- @
-- mkModifierSpec (ByRequest request) (Patch [Add (Pointer [OKey "key"]) (String "value")])
-- mkModifierSpec (ById ruleId) (Patch [Add (Pointer [OKey "key"]) (String "value")])
-- @
--
-- @since 0.1.0.0
mkModifierSpec :: MatcherSpec -> AP.Patch -> ModifierSpec
mkModifierSpec matcher patch =
  MkModifier
    { matcher = matcher
    , jsonPatch = patch
    , modifierId = NoId
    }

-- | Defines how a rule should be matched. Can either be a 'RuleSpec' or the
-- ID of an existing rule.
--
-- @since 0.1.0.0
type MatcherSpec = Matcher RequestSpec

data Matcher req
  = ByRequest req
  | ById RuleId
  deriving (Show, Eq, Functor, Foldable, Traversable)

compileModifier :: ModifierSpec -> Either String Modifier
compileModifier = traverse compileRequest

instance ToJSON ModifierSpec where
  toJSON m = object
    [ "matcher" .= matcher m
    , "jsonPatch" .= jsonPatch m
    ]

instance ToJSON (Matcher RequestSpec) where
  toJSON (ByRequest req) = object
    [ "type" .= ("request" :: T.Text)
    , "value" .= req
    ]
  toJSON (ById ruleId) = object
    [ "type" .= ("rule_id" :: T.Text)
    , "value" .= ruleId
    ]

instance FromJSON ModifierSpec where
  parseJSON = withObject "modifier" $ \o ->
    MkModifier
    <$> o .: "matcher"
    <*> o .: "jsonPatch"
    <*> pure NoId

instance FromJSON (Matcher RequestSpec) where
  parseJSON = withObject "matcher" $ \o -> do
    ty <- o .: "type"
    case ty :: String of
      "request" -> ByRequest <$> o .: "value"
      "rule_id" -> ById <$> o .: "value"
      _ -> fail $ "invalid matcher type: " <> ty
