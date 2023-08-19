{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable
import qualified Data.Text as T
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
  let rule = MkRule
        { request = MkRequest { reqPath = "this/is/a/path" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              }
        , response = MkResponse { respBody = Template ("This is a response" :: T.Text)
                                , respContentType = Nothing
                                }
        }
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_rules")
  resp <- httpBS "GET http://localhost:9000/this/is/a/path"
  getResponseBody resp @?= "This is a response"

addRulesByApi :: DecoyCtx -> Assertion
addRulesByApi dc = do
  let rules = compileRule MkRule
        { request = MkRequest { reqPath = "this/is/another/path"
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              }
        , response = MkResponse { respBody = Template "This is another response"
                                , respContentType = Nothing
                                }
        }
  traverse_ (addRule dc) rules
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "This is another response"

  traverse_ (removeRule dc) rules
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseStatusCode resp2 @?= 404
