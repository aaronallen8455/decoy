{-# LANGUAGE OverloadedStrings #-}
module Decoy
  ( -- * Starting a server instance
    withDecoyServer
  , runDecoyServer
    -- * Modify a running instance
  , mkRule
  , addRule
  , addRules
  , reset
    -- * Types
  , DecoyCtx
  , Rule
  , RuleSpec(..)
  ) where

import           Control.Applicative (asum, (<|>))
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir
import qualified Text.Mustache as Stache

withDecoyServer :: Warp.Port -> Maybe FilePath -> (DecoyCtx -> IO a) -> IO a
withDecoyServer port mRulesFile cont = do
  rules <- loadRulesFile mRulesFile
  initRouterMVar <- newMVar $ mkRouter rules
  Async.withAsync (Warp.run port $ app initRouterMVar mRulesFile) $ \async ->
    cont DC
      { dcRouter = initRouterMVar
      , dcAsync = async
      , dcRulesFile = mRulesFile
      }

runDecoyServer :: Warp.Port -> Maybe FilePath -> IO ()
runDecoyServer port mRulesFile =
  withDecoyServer port mRulesFile $ Async.wait . dcAsync

addRule :: DecoyCtx -> Rule -> IO ()
addRule dc rule = addRules dc [rule]

addRules :: DecoyCtx -> [Rule] -> IO ()
addRules dc rules = modifyMVar_ (dcRouter dc) (pure . insertRules rules)

reset :: DecoyCtx -> IO ()
reset dc =
  putMVar (dcRouter dc) . mkRouter
    =<< loadRulesFile (dcRulesFile dc)

loadRulesFile :: Maybe FilePath -> IO [Rule]
loadRulesFile Nothing = pure []
loadRulesFile (Just rulesFile) = do
  rulesFileExists <- Dir.doesPathExist rulesFile
  if rulesFileExists
     then do
       values <- Aeson.eitherDecodeFileStrict rulesFile
       case traverse mkRule =<< values of
         Left err -> fail $ "Failed to parse rules file: " <> err
         Right rules -> pure rules
     else fail $ "File does not exist: " <> rulesFile

app :: MVar Router -> Maybe FilePath -> Wai.Application
app routerMVar mRulesFile req respHandler = do
  let reqPath = Wai.pathInfo req
      queryMap = M.mapKeys TE.decodeUtf8Lenient
               . fmap (fmap TE.decodeUtf8Lenient)
               . M.fromList
               $ Wai.queryString req
  reqBodyBS <- Wai.strictRequestBody req
  let eReqBodyJson =
        case lookup Http.hContentType $ Wai.requestHeaders req of
          Just ct | ct `elem` ["text/json", "application/json"] ->
            Just <$> Aeson.eitherDecode reqBodyBS
          _ -> Right Nothing

  case reqPath of
    ["_rules"] ->
      case traverse mkRule
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rules -> do
          modifyMVar_ routerMVar (pure . insertRules rules)
          respHandler $ Wai.responseLBS Http.ok200 [] "Rules added"

    ["_reset"] -> do
      putMVar routerMVar . mkRouter =<< loadRulesFile mRulesFile
      respHandler $ Wai.responseLBS Http.ok200 [] "Rules reset"

    _ -> do
      router <- readMVar routerMVar
      respHandler $
        case eReqBodyJson of
          Left err -> Wai.responseLBS Http.badRequest400 []
                        $ "Invalid JSON: " <> BS8.pack err
          Right mReqJson ->
            case matchEndpoint queryMap mReqJson router reqPath of
              Nothing -> Wai.responseLBS Http.notFound404 [] "No rule matched"
              Just resp ->
                Wai.responseLBS Http.ok200 []
                             (LBS.fromStrict $ TE.encodeUtf8 resp)

data Rule = MkRule
  { pathRule :: [PathPart]
  , queryRule :: QueryRules
  , response :: Stache.Template
  }

data DecoyCtx = DC
  { dcRouter :: MVar Router
  , dcAsync :: Async ()
  , dcRulesFile :: Maybe FilePath
  }

type QueryParams = M.Map T.Text (Maybe T.Text)

data PathPart
  = Static T.Text
  | PathParam T.Text

pathFromText :: T.Text -> [PathPart]
pathFromText txt = parsePart <$> T.split (== '/') txt
  where
    parsePart p =
      case T.stripPrefix ":" p of
        Nothing -> Static p
        Just r -> PathParam r

-- pathToText :: [PathPart] -> T.Text
-- pathToText = T.intercalate "/" . map partToText
--   where
--     partToText = \case
--       Static t -> t
--       PathParam t -> ":" <> t

data RuleSpec = MkRuleSpec
  { reqPath :: T.Text
  , reqQueryRules :: Maybe QueryRules
  , reqTemplate :: T.Text
  }

mkRule :: RuleSpec -> Either String Rule
mkRule req = do
  template <- first show . Stache.compileTemplate ""
            $ reqTemplate req
  Right MkRule
    { pathRule = pathFromText $ reqPath req
    , queryRule = fromMaybe M.empty $ reqQueryRules req
    , response = template
    }

instance Aeson.FromJSON RuleSpec where
  parseJSON = Aeson.withObject "Rule Request" $ \o ->
    MkRuleSpec
    <$> o Aeson..: "path"
    <*> o Aeson..:? "query"
    <*> o Aeson..: "response"

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
type PathArgs = M.Map T.Text T.Text

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
insertRules rules router = foldl' (flip insertRule) router rules

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
