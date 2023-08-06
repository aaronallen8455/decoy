{-# LANGUAGE OverloadedStrings #-}
module Decoy.Router
  ( Router
  , mkRouter
  , insertRules
  , insertRule
  , matchEndpoint
  ) where

import           Control.Applicative ((<|>), asum)
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Text.Mustache as Stache

import           Decoy.Rule

data Router = RouterNode
  { staticPaths :: M.Map T.Text Router
  , paramPath :: Maybe Router
  , endpoints :: [Endpoint]
  }

emptyRouter :: Router
emptyRouter = RouterNode M.empty Nothing []

mkRouter :: [Rule] -> Router
mkRouter rules = insertRules rules emptyRouter

data Endpoint = MkEndpoint
  { epPathParamNames :: [T.Text]
  , epRule :: Rule
  }

insertRules :: [Rule] -> Router -> Router
insertRules rules router = foldr insertRule router rules

insertRule :: Rule -> Router -> Router
insertRule rule = go [] (pathRule rule) where
  go pathParams (Static pathPart : rest) router =
    let inner = fromMaybe emptyRouter . M.lookup pathPart $ staticPaths router
        r = go pathParams rest inner
     in router { staticPaths = M.insert pathPart r $ staticPaths router }
  go pathParams (PathParam paramName : rest) router =
    let inner = fromMaybe emptyRouter $ paramPath router
        r = go (paramName : pathParams) rest inner
     in router { paramPath = Just r }
  go pathParams [] router =
    router { endpoints = MkEndpoint
              { epPathParamNames = pathParams
              , epRule = rule
              } : endpoints router
           }

matchEndpoint :: QueryParams -> Maybe Aeson.Value -> Router -> [T.Text] -> Maybe T.Text
matchEndpoint queryParams mReqJson = go [] where
  go params router (part : rest) = static <|> wildcard where
    static = do
      r <- M.lookup part (staticPaths router)
      go params r rest
    wildcard = do
      r <- paramPath router
      go (part : params) r rest
  go params router [] = asum $ do
    ep <- endpoints router
    guard $ matchQuery (queryRule $ epRule ep) queryParams
    let pathParams = M.fromList $ zip (epPathParamNames ep) params
    pure . Just $
      renderTemplate pathParams queryParams mReqJson (response $ epRule ep)

matchQuery :: QueryRules -> QueryParams -> Bool
matchQuery queryRules queryMap = and $ (`map` M.toList queryRules) $
  \(name, mVal) ->
    case M.lookup name queryMap of
      Nothing -> False
      Just qv -> maybe (const True) (maybe False . (==)) mVal qv

type QueryParams = M.Map T.Text (Maybe T.Text)
type PathArgs = M.Map T.Text T.Text

renderTemplate
  :: PathArgs
  -> QueryParams
  -> Maybe Aeson.Value
  -> Stache.Template
  -> T.Text
renderTemplate pathArgs queryMap mReqJson template =
    Stache.substitute template vars
  where
    vars = M.fromList
      [ ("path" :: T.Text, Aeson.toJSON pathArgs)
      , ("query", Aeson.toJSON queryMap)
      , ("body", fromMaybe Aeson.Null mReqJson)
      ]
