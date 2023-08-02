{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative (asum)
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  initRules <- newMVar []
  Warp.runEnv 9000 (app initRules)

app :: Rules -> Wai.Application
app rulesMVar req respHandler = do
  let reqPath = Wai.pathInfo req
      queryMap = M.fromList $ Wai.queryString req
  _reqBodyBS <- Wai.strictRequestBody req
  rules <- readMVar rulesMVar
  respHandler $
    case asum $ matchRule queryMap reqPath <$> rules of
      Nothing -> Wai.responseLBS Http.notFound404 [] "No rule matched"
      Just resp -> Wai.responseLBS Http.ok200 [] resp

data Rule = MkRule
  { pathRule :: [Path]
  , queryRule :: QueryRules
  , response :: ResponseTemplate
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
matchQuery queryRules queryMap = and $ do
  (name, mVal) <- M.toList queryRules
  let mQV = M.lookup name queryMap
  case mQV of
    Nothing -> [False]
    Just qv ->
      pure $ maybe (const True) (maybe False . (==)) mVal qv

matchRule :: QueryParams -> [T.Text] -> Rule -> Maybe LBS.ByteString
matchRule queryParams path rule = do
  pathArgs <- matchPath (pathRule rule) path
  guard $ matchQuery (queryRule rule) queryParams
  Just $ renderTemplate pathArgs queryParams (response rule)

renderTemplate :: PathArgs -> QueryParams -> ResponseTemplate -> LBS.ByteString
renderTemplate _ _ = id

type ResponseTemplate = LBS.ByteString
