{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
module Decoy.Modifier
  ( -- * Types
    ModifierF(..)
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
-- match a rule. You can either define a rule that is specific to the modifier
-- or use the ID of an existing rule. For example, if you want to add a key to
-- a response for a specific test scenario, you could do that by adding a
-- modifier in that test.
--
-- The JSON patch functionality comes from the `aeson-diff` package.
--
-- @since 0.1.0.0
type ModifierSpec = ModifierF NoId RuleSpec

type Modifier = ModifierF NoId Rule
type ModifierWithId = ModifierF ModifierId Rule

newtype ModifierId = MkModifierId Int
  deriving (Enum, Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON)

initModifierId :: ModifierId
initModifierId = MkModifierId 1

data ModifierF id rule = MkModifier
  { matcher :: Matcher rule
  , jsonPatch :: AP.Patch
  , modifierId :: id
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A helper for instantiating a 'ModifierSpec'.
--
-- __Examples:__
--
-- @
-- mkModifierSpec (ByRule rule) (Patch [Add (Pointer [OKey "key"]) (String "value")])
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
type MatcherSpec = Matcher RuleSpec

data Matcher rule
  = ByRule rule
  | ById RuleId
  deriving (Show, Eq, Functor, Foldable, Traversable)

compileModifier :: ModifierSpec -> Either String Modifier
compileModifier = traverse compileRule

instance ToJSON ModifierSpec where
  toJSON m = object
    [ "matcher" .= matcher m
    , "jsonPatch" .= jsonPatch m
    ]

instance ToJSON (Matcher RuleSpec) where
  toJSON (ByRule rule) = object
    [ "type" .= ("rule" :: T.Text)
    , "value" .= rule
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

instance FromJSON (Matcher RuleSpec) where
  parseJSON = withObject "matcher" $ \o -> do
    ty <- o .: "type"
    case ty :: String of
      "rule" -> ByRule <$> o .: "value"
      "rule_id" -> ById <$> o .: "value"
      _ -> fail $ "invalid matcher type: " <> ty
