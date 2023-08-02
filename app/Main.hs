{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative (asum)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
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
  initRules <- newMVar [testRule]
  Warp.runEnv 9000 (app initRules)

testRule :: Rule
testRule = MkRule
  { pathRule = [Static "one", PathParam "two"]
  , queryRule = mempty
  , response = either (error . show) id $
      Stache.compileTemplate "" "{\"hello\": \"world\", \"bodyParam\":\"{{body.two}}\", \"pathParam\":\"{{path.two}}\"}"
  }

app :: Rules -> Wai.Application
app rulesMVar req respHandler = do
  let reqPath = Wai.pathInfo req
      queryMap = M.fromList $ Wai.queryString req
  reqBodyBS <- Wai.strictRequestBody req
  let eReqBodyJson =
        case lookup Http.hContentType $ Wai.requestHeaders req of
          Just ct | ct `elem` ["text/json", "application/json"] ->
            Just <$> Aeson.eitherDecode reqBodyBS
          _ -> Right Nothing
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

type QueryParams = M.Map BS.ByteString (Maybe BS.ByteString)

data Path
  = Static T.Text
  | PathParam T.Text

type QueryRules = M.Map BS.ByteString (Maybe BS.ByteString) -- TODO ignore vs require no value
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
      , ("query", Aeson.toJSON . M.mapKeys TE.decodeUtf8Lenient
                    $ M.mapMaybe (fmap TE.decodeUtf8Lenient) queryMap)
      , ("body", fromMaybe Aeson.Null mReqJson)
      ]
