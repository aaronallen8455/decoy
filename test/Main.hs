{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import           Data.Aeson.QQ
import           Data.Foldable
import qualified Data.Text as T
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
  ]

addRulesByRequest :: Assertion
addRulesByRequest = do
  let rule = MkRule
        { request = MkRequest { reqPath = "this/is/a/path" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules = mempty
                              }
        , response = MkResponse { respBody = Template ("This is a response" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        } :: RuleSpec
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
                              , reqBodyRules = mempty
                              }
        , response = MkResponse { respBody = Template "This is another response"
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        }
  traverse_ (addRule dc) rules
  resp <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseBody resp @?= "This is another response"

  traverse_ (removeRule dc) rules
  resp2 <- httpBS "GET http://localhost:9000/this/is/another/path"
  getResponseStatusCode resp2 @?= 404

bodyRegex :: Assertion
bodyRegex = do
  let rule = MkRule
        { request = MkRequest { reqPath = "this/is/a/path" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules =
                                [ MkBodyRule
                                  { regex = "test.ng"
                                  , jsonPathOpts = Nothing
                                  }
                                ]
                              }
        , response = MkResponse { respBody = Template ("regex matched" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        } :: RuleSpec
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_rules")
  resp <- httpBS (setRequestBody "testing" "GET http://localhost:9000/this/is/a/path")
  getResponseBody resp @?= "regex matched"

jsonRegex :: Assertion
jsonRegex = do
  let rule = MkRule
        { request = MkRequest { reqPath = "this/is/a/path" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules =
                                [ MkBodyRule
                                  { regex = "^hello$"
                                  , jsonPathOpts = Just MkJsonPathOpts
                                    { jsonPath = "$.field.field2"
                                    , allMatch = True
                                    }
                                  }
                                ]
                              }
        , response = MkResponse { respBody = Template ("json regex matched" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        } :: RuleSpec
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_rules")
  resp <- httpBS (setRequestBodyJSON
                    [aesonQQ| {field:{field2:"hello"}} |]
                    "POST http://localhost:9000/this/is/a/path")
  getResponseBody resp @?= "json regex matched"

requestMethod :: Assertion
requestMethod = do
  let rules =
        [ MkRule
        { request = MkRequest { reqPath = "reqmeth" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Just "GET"
                              , reqContentType = Nothing
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("get" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        }
        , MkRule
        { request = MkRequest { reqPath = "reqmeth" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Just "POST"
                              , reqContentType = Nothing
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("post" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        } ] :: [RuleSpec]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_rules")
  resp <- httpBS "POST http://localhost:9000/reqmeth"
  getResponseBody resp @?= "post"

reqCT :: Assertion
reqCT = do
  let rules =
        [ MkRule
        { request = MkRequest { reqPath = "reqct" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Just "text/plain"
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("text" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        }
        , MkRule
        { request = MkRequest { reqPath = "reqct" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Just "application/json"
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("json" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Nothing
                                }
        } ] :: [RuleSpec]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_rules")
  resp <- httpBS (setRequestBodyJSON [aesonQQ| {field: true} |] "POST http://localhost:9000/reqct")
  getResponseBody resp @?= "json"

respCT :: Assertion
respCT = do
  let rules =
        [ MkRule
        { request = MkRequest { reqPath = "respct" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("text" :: T.Text)
                                , respContentType = Just "text/plain"
                                , respStatusCode = Nothing
                                }
        }
        , MkRule
        { request = MkRequest { reqPath = "respct" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("\"json\"" :: T.Text)
                                , respContentType = Just "application/json"
                                , respStatusCode = Nothing
                                }
        } ] :: [RuleSpec]
  _ <- httpNoBody (setRequestBodyJSON rules "POST http://localhost:9000/_rules")
  resp <- httpJSON "GET http://localhost:9000/respct"
  getResponseBody resp @?= Aeson.String "json"
  resp2 <- httpBS (addRequestHeader H.hAccept "text/plain" "GET http://localhost:9000/respct")
  getResponseBody resp2 @?= "text"

respStatus :: Assertion
respStatus = do
  let rule =
        MkRule
        { request = MkRequest { reqPath = "statusc" :: T.Text
                              , reqQuery = mempty
                              , reqMethod = Nothing
                              , reqContentType = Nothing
                              , reqBodyRules = []
                              }
        , response = MkResponse { respBody = Template ("text" :: T.Text)
                                , respContentType = Nothing
                                , respStatusCode = Just 500
                                }
        } :: RuleSpec
  _ <- httpNoBody (setRequestBodyJSON [rule] "POST http://localhost:9000/_rules")
  resp <- httpBS "GET http://localhost:9000/statusc"
  getResponseStatusCode resp @?= 500
