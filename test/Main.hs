{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable
import           Test.Tasty
import           Test.Tasty.HUnit
import           Network.HTTP.Simple

import           Decoy

main :: IO ()
main = withDecoyServer 9000 Nothing $ defaultMain . tests

tests :: DecoyCtx -> TestTree
tests dc = testGroup "Tests"
  [ testCase "Add rule via request" addRulesByRequest
  , testCase "Add rule via API" (addRulesByApi dc)
  ]

addRulesByRequest :: Assertion
addRulesByRequest = do
  let rules = [ MkRuleSpec
                  { rsPath = "this/is/a/path"
                  , rsQueryRules = mempty
                  , rsTemplate = "This is a response"
                  , rsResponseContentType = Nothing
                  , rsRequestContentType = Nothing
                  , rsMethod = Nothing
                  }
              ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_rules")
  resp <- httpBS "GET http://localhost:9000/this/is/a/path"
  getResponseBody resp @?= "This is a response"

addRulesByApi :: DecoyCtx -> Assertion
addRulesByApi dc = do
  let rules = traverse mkRule
              [ MkRuleSpec
                  { rsPath = "this/is/another/path"
                  , rsQueryRules = mempty
                  , rsTemplate = "This is another response"
                  , rsResponseContentType = Nothing
                  , rsRequestContentType = Nothing
                  , rsMethod = Nothing
                  }
              ]
  traverse_ (addRules dc) rules
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "This is another response"

  traverse_ (removeRules dc) rules
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseStatusCode resp2 @?= 404
