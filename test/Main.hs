{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.Patch
import           Data.Aeson.Pointer
import           Data.Aeson.QQ
import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit
import           Network.HTTP.Simple
import qualified Network.HTTP.Types.Header as H

import           Decoy

main :: IO ()
main = withDecoyServer 9000 ["test_rules.json"] $ defaultMain . tests

tests :: DecoyCtx -> TestTree
tests dc = testGroup "Tests"
  [ testCase "Add rule via request" addRulesByRequest
  , testCase "Add rule via API" (addRulesByApi dc)
  , testCase "Add modifier via request" addModifiersByRequest
  , testCase "add modifier via API" (addModifiersByApi dc)
  , testCase "Body regex rule" bodyRegex
  , testCase "JSON body regex" jsonRegex
  , testCase "Request method" requestMethod
  , testCase "Request content type" reqCT
  , testCase "Response content type" respCT
  , testCase "Status code" respStatus
  , testCase "Path param" pathParam
  , testCase "Query string match" queryStringMatch
  , testCase "Query string arg" queryStringArg
  , testCase "Remove rule via request" removeRuleByRequest
  , testCase "Deals with slashes" slashes
  , testCase "Empty component" emptyComponent
  , testCase "Header rules" headerMatch
  , testCase "Rules file" rulesFile
  -- TODO reset endpoint
  ]

addRulesByRequest :: Assertion
addRulesByRequest = do
  let rule = mkRuleSpec "this/is/a/path" (Template "This is a response")
  ruleIds <- getResponseBody <$>
    httpJSON (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/this/is/a/path"
  _ <- httpNoBody (setRequestBodyJSON (ruleIds :: Aeson.Value) "POST http://localhost:9000/_remove-rules")
  getResponseBody resp @?= "This is a response"

addRulesByApi :: DecoyCtx -> Assertion
addRulesByApi dc = do
  let rule = fromRight undefined . compileRule $ mkRuleSpec "this/is/another/path" $ Template "This is another response"
  [rId] <- addRules dc [rule]
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "This is another response"

  removeRules dc [rId]
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseStatusCode resp2 @?= 404

addModifiersByRequest :: Assertion
addModifiersByRequest = do
  let rule = mkRuleSpec "this/is/a/path" (Template "{\"foo\":true}")
  [ruleId] <- getResponseBody <$> httpJSON (setRequestBodyJSON [rule] "POST http://localhost:9000/_add-rules")
  let modifiers =
        [ mkModifierSpec (ByRequest $ request rule) (Patch [Add (Pointer [OKey "bar"]) (Aeson.String "bar")])
        , mkModifierSpec (ById ruleId) (Patch [Add (Pointer [OKey "baz"]) (Aeson.String "baz")])
        ]
  modIds <- getResponseBody <$> httpJSON (setRequestBodyJSON modifiers "POST http://localhost:9000/_add-modifiers")
  resp <- httpBS "GET http://localhost:9000/this/is/a/path"
  getResponseBody resp @?= "{\"bar\":\"bar\",\"baz\":\"baz\",\"foo\":true}"
  _ <- httpNoBody (setRequestBodyJSON (modIds :: Aeson.Value) "POST http://localhost:9000/_remove-modifiers")
  resp2 <- httpBS "GET http://localhost:9000/this/is/a/path"
  getResponseBody resp2 @?= "{\"foo\":true}"

addModifiersByApi :: DecoyCtx -> Assertion
addModifiersByApi dc = do
  let ruleSpec = mkRuleSpec "this/is/another/path" $ Template "{\"foo\":true}"
  let rule = fromRight undefined $ compileRule ruleSpec
  [ruleId] <- addRules dc [rule]
  let modifiers = fromRight undefined $ traverse compileModifier
        [ mkModifierSpec (ByRequest $ request ruleSpec) (Patch [Add (Pointer [OKey "bar"]) (Aeson.String "bar")])
        , mkModifierSpec (ById ruleId) (Patch [Add (Pointer [OKey "baz"]) (Aeson.String "baz")])
        ]
  modIds <- addModifiers dc modifiers
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "{\"bar\":\"bar\",\"baz\":\"baz\",\"foo\":true}"

  removeModifiers dc modIds
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp2 @?= "{\"foo\":true}"

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
        [ addQueryRule (MkKeyValRule "query" (Just "one")) . mkRuleSpec "query/string" $ Template "one"
        , addQueryRule (MkKeyValRule "query" (Just "two")) . mkRuleSpec "query/string" $ Template "two"
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

removeRuleByRequest :: Assertion
removeRuleByRequest = do
  let rules =
        [ mkRuleSpec "delete/me" $ Template "one"
        , mkRuleSpec "delete/me2" $ Template "two"
        ]
  rIds :: [RuleId] <- getResponseBody <$>
    httpJSON (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/delete/me"
  getResponseBody resp @?= "one"
  _ <- httpNoBody (setRequestBodyJSON rIds "POST http://localhost:9000/_remove-rules")
  resp2 <- httpBS "GET http://localhost:9000/delete/me2"
  getResponseStatusCode resp2 @?= 404

slashes :: Assertion
slashes = do
  let rules =
        [ mkRuleSpec "trail/slash/" $ Template "one"
        , mkRuleSpec "/lead/slash" $ Template "two"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/trail/slash"
  getResponseBody resp @?= "one"
  resp3 <- httpBS "GET http://localhost:9000/trail/slash/"
  getResponseBody resp3 @?= "one"
  resp4 <- httpBS "GET http://localhost:9000/lead/slash"
  getResponseBody resp4 @?= "two"
  resp5 <- httpBS "GET http://localhost:9000/lead/slash/"
  getResponseBody resp5 @?= "two"

emptyComponent :: Assertion
emptyComponent = do
  let rules =
        [ mkRuleSpec "empty//component" $ Template "one"
        , mkRuleSpec "empty/component//" $ Template "two"
        , mkRuleSpec "//empty/component" $ Template "three"
        , mkRuleSpec "/" $ Template "four"
        , mkRuleSpec "//" $ Template "five"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS "GET http://localhost:9000/empty//component"
  getResponseBody resp @?= "one"
  resp2 <- httpBS "GET http://localhost:9000/empty/component/"
  getResponseStatusCode resp2 @?= 404
  resp3 <- httpBS "GET http://localhost:9000/empty/component//"
  getResponseBody resp3 @?= "two"
  resp4 <- httpBS "GET http://localhost:9000//empty/component"
  getResponseBody resp4 @?= "three"
  resp5 <- httpBS "GET http://localhost:9000/"
  getResponseBody resp5 @?= "four"
  resp6 <- httpBS "GET http://localhost:9000///"
  getResponseBody resp6 @?= "five"

headerMatch :: Assertion
headerMatch = do
  let rules =
        [ addHeaderRule (MkKeyValRule "header" (Just "one")) . mkRuleSpec "header/check" $ Template "one"
        , addHeaderRule (MkKeyValRule "header" (Just "two")) . mkRuleSpec "header/check" $ Template "two"
        , addHeaderRule (MkKeyValRule "header" Nothing) . mkRuleSpec "header/check2" $ Template "ok"
        ]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_add-rules")
  resp <- httpBS $ addRequestHeader "header" "two" "GET http://localhost:9000/header/check/"
  getResponseBody resp @?= "two"
  resp2 <- httpBS $ addRequestHeader "header" "blah" "GET http://localhost:9000/header/check2"
  getResponseBody resp2 @?= "ok"
  resp3 <- httpBS "GET http://localhost:9000/header/check2"
  getResponseStatusCode resp3 @?= 404

rulesFile :: Assertion
rulesFile = do
  resp <- httpBS "GET http://localhost:9000/path/of/righteousness"
  getResponseBody resp @?= "hey"
