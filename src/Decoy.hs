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

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir

import           Decoy.Router (Router, matchEndpoint, mkRouter, insertRules)
import           Decoy.Rule (Rule, mkRule, RuleSpec(..))

data DecoyCtx = DC
  { dcRouter :: MVar Router
  , dcAsync :: Async ()
  , dcRulesFile :: Maybe FilePath
  }

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
