{-# LANGUAGE LambdaCase #-}

import qualified Data.Aeson as Aeson
import           Data.Functor.Const
import           Data.Scientific (scientific)
import qualified Data.Text as T
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Decoy.Types

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree = testGroup "JSON"
  [ testProperty "RuleSpec round trips" . property $ do
      x <- forAll ruleSpecGen
      tripping x Aeson.encode Aeson.eitherDecode
  , testProperty "ModifierSpec round trips" . property $ do
      x <- forAll modifierSpecGen
      tripping x Aeson.encode Aeson.eitherDecode
  ]

ruleSpecGen :: Gen RuleSpec
ruleSpecGen =
  MkRule
  <$> requestSpecGen
  <*> responseGen
  <*> pure NoId

requestSpecGen :: Gen RequestSpec
requestSpecGen =
  MkRequest
  <$> Gen.text (Range.linear 0 10) Gen.alpha
  <*> Gen.list (Range.linear 0 10) keyValRuleGen
  <*> Gen.maybe (Gen.text (Range.linear 0 10) Gen.alpha)
  <*> Gen.maybe (Gen.text (Range.linear 0 10) Gen.alpha)
  <*> Gen.list (Range.linear 0 10) keyValRuleGen
  <*> Gen.list (Range.linear 0 10) bodyRuleGen

bodyRuleGen :: Gen (BodyRule T.Text)
bodyRuleGen =
  MkBodyRule
  <$> Gen.maybe jsonPathOptsGen
  <*> Gen.text (Range.linear 0 10) Gen.alpha

jsonPathOptsGen :: Gen (JsonPathOpts T.Text)
jsonPathOptsGen =
  MkJsonPathOpts
  <$> Gen.text (Range.linear 0 10) Gen.alpha
  <*> Gen.bool

keyValRuleGen :: Gen KeyValRule
keyValRuleGen =
  MkKeyValRule
  <$> Gen.text (Range.linear 0 10) Gen.alpha
  <*> Gen.maybe (Gen.text (Range.linear 0 10) Gen.unicode)

modifierSpecGen :: Gen ModifierSpec
modifierSpecGen =
  MkModifier
  <$> matcherSpecGen
  <*> pure mempty
  <*> pure NoId

matcherSpecGen :: Gen MatcherSpec
matcherSpecGen = Gen.choice . getConst $
  choose matcherToEither
    (mkChoice ByRequest [requestSpecGen])
    (mkChoice ById [MkRuleId <$> Gen.int (Range.linear 0 10)])

matcherToEither :: MatcherSpec -> Either RequestSpec RuleId
matcherToEither = \case
  ByRequest r -> Left r
  ById i -> Right i

responseGen :: Gen (Response T.Text)
responseGen =
  MkResponse
  <$> responseBodyGen
  <*> Gen.maybe (Gen.text (Range.linear 0 10) Gen.alpha)
  <*> Gen.maybe (pure 200)
  <*> Gen.maybe (scientific <$> Gen.integral (Range.linear 0 200)
                            <*> Gen.int (Range.linear (-4) 4)
                )

responseBodyGen :: Gen (ResponseBody T.Text)
responseBodyGen = Gen.choice . getConst $
  choose responseBodyToEither
    (mkChoice File [Gen.string (Range.linear 0 10) Gen.unicode])
    (choose id
      (mkChoice Template [Gen.text (Range.linear 0 10) Gen.unicode])
      (mkChoice (const NoBody) [pure ()])
    )

responseBodyToEither :: ResponseBody T.Text -> Either FilePath (Either T.Text ())
responseBodyToEither = \case
  File f -> Left f
  Template t -> Right (Left t)
  NoBody -> Right (Right ())

choose :: Monoid m => (s -> Either a b) -> Const m a -> Const m b -> Const m s
choose _ (Const a) (Const b) = Const $ a <> b

mkChoice :: (a -> s) -> [Gen a] -> Const [Gen s] a
mkChoice inject m = Const $ fmap inject <$> m
