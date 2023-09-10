{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ
import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit
import           Network.HTTP.Simple
import qualified Network.HTTP.Types.Header as H

import           Decoy

main :: IO ()
main = withDecoyServer 9000 Nothing $ defaultMain . tests

tests :: DecoyCtx -> TestTree
tests dc = testGroup "Tests"
  [ testCase "Add rule via request" addRulesByRequest
  , testCase "Add rule via API" (addRulesByApi dc)
  , testCase "Body regex rule" bodyRegex
  , testCase "JSON body regex" jsonRegex
  , testCase "Request method" requestMethod
  , testCase "Request content type" reqCT
  , testCase "Response content type" respCT
  , testCase "Status code" respStatus
  , testCase "Path param" pathParam
  , testCase "Query string match" queryStringMatch
  , testCase "Query string arg" queryStringArg
  ]

addRulesByRequest :: Assertion
addRulesByRequest = do
  let rule = mkRuleSpec "this/is/a/path" (Template "This is a response")
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/this/is/a/path"
  getResponseBody resp @?= "This is a response"

addRulesByApi :: DecoyCtx -> Assertion
addRulesByApi dc = do
  let rule = fromRight undefined . compileRule $ mkRuleSpec "this/is/another/path" $ Template "This is another response"
  rId <- addRule dc rule
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "This is another response"

  removeRule dc rId
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseStatusCode resp2 @?= 404

bodyRegex :: Assertion
bodyRegex = do
  let rule = addBodyRule MkBodyRule { regex = "test.ng"
                                    , jsonPathOpts = Nothing
                                    }
           $ mkRuleSpec "this/is/a/path" (Template "regex matched")
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS (setRequestBody "testing" "GET http://localhost:9000/this/is/a/path")
  getResponseBody resp @?= "regex matched"

jsonRegex :: Assertion
jsonRegex = do
  let rule = addBodyRule MkBodyRule
                { regex = "^hello$"
                , jsonPathOpts = Just MkJsonPathOpts
                  { jsonPath = "$.field.field2"
                  , allMatch = True
                  }
                }
           $ mkRuleSpec "this/is/a/path" $ Template "json regex matched"
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS (setRequestBodyJSON
                    [aesonQQ| {field:{field2:"hello"}} |]
                    "POST http://localhost:9000/this/is/a/path")
  getResponseBody resp @?= "json regex matched"

requestMethod :: Assertion
requestMethod = do
  let rules =
        [ setReqMethod "GET" . mkRuleSpec "reqmeth" $ Template "get"
        , setReqMethod "POST" . mkRuleSpec "reqmeth" $ Template "post"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS "POST http://localhost:9000/reqmeth"
  getResponseBody resp @?= "post"

reqCT :: Assertion
reqCT = do
  let rules =
        [ setReqContentType "text/plain" . mkRuleSpec "reqct" $ Template "text"
        , setReqContentType "application/json" . mkRuleSpec "reqct" $ Template "json"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS (setRequestBodyJSON [aesonQQ| {field: true} |] "POST http://localhost:9000/reqct")
  getResponseBody resp @?= "json"

respCT :: Assertion
respCT = do
  let rules =
        [ setRespContentType "text/plain" . mkRuleSpec "respct" $ Template "text"
        , setRespContentType "application/json" . mkRuleSpec "respct" $ Template "\"json\""
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpJSON "GET http://localhost:9000/respct"
  getResponseBody resp @?= Aeson.String "json"
  resp2 <- httpBS (addRequestHeader H.hAccept "text/plain" "GET http://localhost:9000/respct")
  getResponseBody resp2 @?= "text"

respStatus :: Assertion
respStatus = do
  let rule = setStatusCode 500 . mkRuleSpec "statusc" $ Template "text"
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/statusc"
  getResponseStatusCode resp @?= 500

pathParam :: Assertion
pathParam = do
  let rule = mkRuleSpec "param/path/:param" $ Template "param: {{path.param}}"
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/param/path/yo"
  getResponseBody resp @?= "param: yo"

queryStringMatch :: Assertion
queryStringMatch = do
  let rules =
        [ addQueryRule (MkQueryRule "query" (Just "one")) . mkRuleSpec "query/string" $ Template "one"
        , addQueryRule (MkQueryRule "query" (Just "two")) . mkRuleSpec "query/string" $ Template "two"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/query/string?query=two"
  getResponseBody resp @?= "two"

queryStringArg :: Assertion
queryStringArg = do
  let rule = mkRuleSpec "query/string/arg" $ Template "query: {{query.arg}}"
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/query/string/arg?arg=sup"
  getResponseBody resp @?= "query: sup"
