{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative (asum)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Mustache as Stache

main :: IO ()
main = do
  initRules <- newMVar []
  Warp.runEnv 9000 (app initRules)

app :: Rules -> Wai.Application
app rulesMVar req respHandler = do
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
      case compileRule =<< Aeson.parseEither Aeson.parseJSON
                       =<< maybe (Left "No body") Right
                       =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rule -> do
          modifyMVar_ rulesMVar (pure . (rule :))
          respHandler $ Wai.responseLBS Http.ok200 [] "Rule added"
    _ -> do
      rules <- readMVar rulesMVar
      respHandler $
        case eReqBodyJson of
          Left err -> Wai.responseLBS Http.badRequest400 []
                        $ "Invalid JSON: " <> BS8.pack err
          Right mReqJson ->
            case asum $ matchRule queryMap reqPath mReqJson <$> rules of
              Nothing -> Wai.responseLBS Http.notFound404 [] "No rule matched"
              Just resp ->
                Wai.responseLBS Http.ok200 []
                             (LBS.fromStrict $ TE.encodeUtf8 resp)

data Rule = MkRule
  { pathRule :: [Path]
  , queryRule :: QueryRules
  , response :: Stache.Template
  }

type Rules = MVar [Rule]

type QueryParams = M.Map T.Text (Maybe T.Text)

data Path
  = Static T.Text
  | PathParam T.Text

pathFromText :: T.Text -> [Path]
pathFromText txt = parsePart <$> T.split (== '/') txt
  where
    parsePart p =
      case T.stripPrefix ":" p of
        Nothing -> Static p
        Just r -> PathParam r

pathToText :: [Path] -> T.Text
pathToText = T.intercalate "/" . map partToText
  where
    partToText = \case
      Static t -> t
      PathParam t -> ":" <> t

data RuleRequest = MkRuleRequest
  { reqPath :: T.Text
  , reqQueryRules :: Maybe QueryRules
  , reqTemplate :: T.Text
  }

compileRule :: RuleRequest -> Either String Rule
compileRule req = do
  template <- first show . Stache.compileTemplate ""
            $ reqTemplate req
  Right MkRule
    { pathRule = pathFromText $ reqPath req
    , queryRule = fromMaybe M.empty $ reqQueryRules req
    , response = template
    }

instance Aeson.FromJSON RuleRequest where
  parseJSON = Aeson.withObject "Rule Request" $ \o ->
    MkRuleRequest
    <$> o Aeson..: "path"
    <*> o Aeson..:? "query"
    <*> o Aeson..: "response"

type QueryRules = M.Map T.Text (Maybe T.Text) -- TODO ignore vs require no value
type PathArgs = M.Map T.Text T.Text

matchPath :: [Path] -> [T.Text] -> Maybe PathArgs
matchPath (Static path : pathRest) (r : reqRest)
  | path == r = matchPath pathRest reqRest
  | otherwise = Nothing
matchPath (PathParam paramName : pathRest) (r : reqRest)
  = M.insert paramName r <$> matchPath pathRest reqRest
matchPath [] (_ : _) = Nothing
matchPath (_ : _) [] = Nothing
matchPath [] [] = Just M.empty

matchQuery :: QueryRules -> QueryParams -> Bool
matchQuery queryRules queryMap = and $ (`map` M.toList queryRules) $
  \(name, mVal) ->
    case M.lookup name queryMap of
      Nothing -> False
      Just qv -> maybe (const True) (maybe False . (==)) mVal qv

matchRule :: QueryParams -> [T.Text] -> Maybe Aeson.Value -> Rule -> Maybe T.Text
matchRule queryParams path mReqJson rule = do
  pathArgs <- matchPath (pathRule rule) path
  guard $ matchQuery (queryRule rule) queryParams
  Just $ renderTemplate pathArgs queryParams mReqJson (response rule)

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
