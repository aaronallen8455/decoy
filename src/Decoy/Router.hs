{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Decoy.Router
  ( Router
  , mkRouter
  , addRouterRules
  , addRouterRule
  , removeRouterRules
  , removeRouterRule
  , matchEndpoint
  ) where

import           Control.Applicative ((<|>), empty)
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import qualified Data.JSONPath as JP
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Http
import qualified Text.Mustache as Stache
import qualified Text.Regex.TDFA as Regex

import           Decoy.Rule

data Router = RouterNode
  { staticPaths :: M.Map T.Text Router
  , paramPath :: Maybe Router
  , endpoints :: [Endpoint]
  }

emptyRouter :: Router
emptyRouter = RouterNode M.empty Nothing []

mkRouter :: [Rule] -> Router
mkRouter rules = addRouterRules rules emptyRouter

data Endpoint = MkEndpoint
  { epPathParamNames :: [T.Text]
  , epRule :: Rule
  }

addRouterRules :: [Rule] -> Router -> Router
addRouterRules rules router = foldr addRouterRule router rules

addRouterRule :: Rule -> Router -> Router
addRouterRule rule = go [] (reqPath $ request rule) where
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

removeRouterRules :: [Rule] -> Router -> Router
removeRouterRules rules router = foldr removeRouterRule router rules

removeRouterRule :: Rule -> Router -> Router
removeRouterRule rule = go (reqPath $ request rule) where
  go [] router =
    let matches r = void r == void rule
     in router { endpoints = filter (not . matches . epRule) $ endpoints router }
  go (Static path : rest) router =
    router { staticPaths = M.alter (fmap $ go rest) path
                         $ staticPaths router
           }
  go (PathParam _ : rest) router =
    router { paramPath = go rest <$> paramPath router }

matchEndpoint
  :: QueryParams
  -> LBS.ByteString
  -> Maybe Aeson.Value
  -> Http.RequestHeaders
  -> Http.Method
  -> Router
  -> [T.Text]
  -> Maybe (ResponseBody T.Text, Maybe T.Text)
matchEndpoint queryParams rawBody mReqJson reqHeaders reqMeth = go [] where
  go params router (part : rest) = static <|> wildcard where
    static = do
      r <- M.lookup part (staticPaths router)
      go params r rest
    wildcard = do
      r <- paramPath router
      go (part : params) r rest
  go params router [] = asum $ do
    ep <- endpoints router

    guard $ matchQuery (reqQuery . request $ epRule ep) queryParams

    for_ (reqContentType . request $ epRule ep) $ \ct ->
      maybe empty (guard . (ct `T.isInfixOf`) . TE.decodeUtf8Lenient)
        $ lookup Http.hContentType reqHeaders

    for_ (respContentType . response $ epRule ep) $ \a ->
      maybe empty (guard . (a `T.isInfixOf`) . TE.decodeUtf8Lenient)
        $ lookup Http.hAccept reqHeaders

    for_ (reqMethod . request $ epRule ep)
      $ guard . (== reqMeth) . TE.encodeUtf8

    guard $ matchBody (reqBodyRules (request $ epRule ep)) rawBody mReqJson

    let pathParams = M.fromList $ zip (epPathParamNames ep) params
    pure $ Just
      ( renderTemplate pathParams queryParams mReqJson
          <$> respBody (response $ epRule ep)
      , respContentType . response $ epRule ep
      )

matchBody :: [BodyRule [JP.JSONPathElement]] -> LBS.ByteString -> Maybe Aeson.Value -> Bool
matchBody bodyRules rawBody mBodyJson = all match bodyRules where
  match rule = case jsonPathOpts rule of
    Nothing -> rawBody Regex.=~ regex rule
    Just opts | Just bodyJson <- mBodyJson
              , let op = if allMatch opts
                            then (\p xs -> all p xs && not (null xs))
                            else any
      -> (`op` JP.executeJSONPath (jsonPath opts) bodyJson) $ \case
          Aeson.String txt -> txt Regex.=~ regex rule
          other -> Aeson.encode other Regex.=~ regex rule
    _ -> False

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
