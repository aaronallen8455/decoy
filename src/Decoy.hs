{-# LANGUAGE OverloadedStrings #-}
module Decoy
  ( -- * Starting a server instance
    withDecoyServer
  , runDecoyServer
    -- * Modify a running instance
  , addRule
  , addRules
  , removeRule
  , removeRules
  , reset
    -- * Types
  , DecoyCtx(..)
  , R.Router
  , module Rule
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir

import qualified Decoy.Router as R
import           Decoy.Rule as Rule

data DecoyCtx = DC
  { dcRouter :: MVar R.Router
  , dcAsync :: Async ()
  , dcRulesFile :: Maybe FilePath
  }

withDecoyServer :: Warp.Port -> Maybe FilePath -> (DecoyCtx -> IO a) -> IO a
withDecoyServer port mRulesFile cont = do
  rules <- loadRulesFile mRulesFile
  initRouterMVar <- newMVar $ R.mkRouter rules
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
addRules dc rules = modifyMVar_ (dcRouter dc) (pure . R.addRouterRules rules)

removeRule :: DecoyCtx -> Rule -> IO ()
removeRule dc rule = removeRules dc [rule]

removeRules :: DecoyCtx -> [Rule] -> IO ()
removeRules dc rules = modifyMVar_ (dcRouter dc) (pure . R.removeRouterRules rules)

reset :: DecoyCtx -> IO ()
reset dc =
  putMVar (dcRouter dc) . R.mkRouter
    =<< loadRulesFile (dcRulesFile dc)

loadRulesFile :: Maybe FilePath -> IO [Rule]
loadRulesFile Nothing = pure []
loadRulesFile (Just rulesFile) = do
  rulesFileExists <- Dir.doesPathExist rulesFile
  if rulesFileExists
     then do
       values <- Aeson.eitherDecodeFileStrict rulesFile
       case traverse compileRule =<< values of
         Left err -> fail $ "Failed to parse rules file: " <> err
         Right rules -> pure rules
     else fail $ "File does not exist: " <> rulesFile

app :: MVar R.Router -> Maybe FilePath -> Wai.Application
app routerMVar mRulesFile req respHandler = do
  let reqPath = Wai.pathInfo req
      queryMap = M.mapKeys TE.decodeUtf8Lenient
               . fmap (fmap TE.decodeUtf8Lenient)
               . M.fromList
               $ Wai.queryString req
      reqHeaders = Wai.requestHeaders req
      reqMethod = Wai.requestMethod req
  reqBodyBS <- Wai.strictRequestBody req
  let eReqBodyJson =
        case lookup Http.hContentType reqHeaders of
          Just ct | "json" `BS.isInfixOf` ct ->
            Just <$> Aeson.eitherDecode reqBodyBS
          _ -> Right Nothing

  case reqPath of
    ["_rules"] ->
      case traverse compileRule
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rules -> do
          modifyMVar_ routerMVar (pure . R.addRouterRules rules)
          respHandler $ Wai.responseLBS Http.ok200 [] "Rules added"

    ["_reset"] -> do
      putMVar routerMVar . R.mkRouter =<< loadRulesFile mRulesFile
      respHandler $ Wai.responseLBS Http.ok200 [] "Rules reset"

    _ -> do
      router <- readMVar routerMVar
      respHandler =<<
        case eReqBodyJson of
          Left err -> pure . Wai.responseLBS Http.badRequest400 []
                        $ "Invalid JSON: " <> BS8.pack err
          Right mReqJson ->
            case R.matchEndpoint queryMap reqBodyBS mReqJson reqHeaders reqMethod router reqPath of
              Nothing -> pure $ Wai.responseLBS Http.notFound404 [] "No rule matched"
              Just matched -> handleMatchedEndpoint matched

handleMatchedEndpoint :: R.MatchedEndpoint -> IO Wai.Response
handleMatchedEndpoint R.MkMatchedEndpoint{ R.responseBody, R.contentType, R.statusCode } =
  case responseBody of
    Template resp -> pure $
      Wai.responseLBS respCode
        respHeaders
        (LBS.fromStrict $ TE.encodeUtf8 resp)
    File file -> do
      exists <- Dir.doesFileExist file
      if not exists
         then pure $
           Wai.responseLBS Http.notFound404 [] $ "File not found: " <> BS8.pack file
         else do
           content <- LBS.readFile file
           pure $ Wai.responseLBS respCode respHeaders content
  where
    respHeaders =
      [ (Http.hContentType, TE.encodeUtf8 ct) | Just ct <- [contentType] ]
    respCode = case statusCode of
      Nothing -> Http.ok200
      Just sc ->
        case M.lookup (fromIntegral sc) allCodes of
          Nothing -> Http.mkStatus (fromIntegral sc) mempty
          Just c -> c
    allCodes = M.fromList $ (\x -> (Http.statusCode x, x)) <$> [minBound .. maxBound]
