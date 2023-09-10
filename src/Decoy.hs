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
  , withRule
  , withRules
  , reset
    -- * Types
  , DecoyCtx(..)
  , R.Router
  , module Rule
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar
import           Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir
import qualified Text.Mustache as Stache

import qualified Decoy.Router as R
import           Decoy.Rule as Rule

-- | Holds the state related to a decoy server instance.
--
-- @since 0.1.0.0
data DecoyCtx = DC
  { dcRouter :: MVar R.Router
  , dcRuleIds :: MVar (M.Map Rule.RuleId [Rule.PathPart])
  , dcRulesFile :: Maybe FilePath
  , dcFileCache :: MVar FileCache
  }

type FileCache = M.Map FilePath Stache.Template

-- | Run a decoy server in a child thread given a port and optional rules file.
--
-- __Example:__
--
-- @
-- withDecoyServer 8080 Nothing $ \dc -> do
--    addRules dc someRules
--    _ <- httpBS "GET http://localhost:8080/some/path"
-- @
--
-- @since 0.1.0.0
withDecoyServer :: Warp.Port -> Maybe FilePath -> (DecoyCtx -> IO a) -> IO a
withDecoyServer port mRulesFile cont = do
  rules <- loadRulesFile mRulesFile
  initRouterMVar <- newMVar $ R.mkRouter rules
  initFileCache <- newMVar mempty
  initRuleIds <- newMVar mempty
  let dc = DC
        { dcRouter = initRouterMVar
        , dcRuleIds = initRuleIds
        , dcRulesFile = mRulesFile
        , dcFileCache = initFileCache
        }
  Async.withAsync (Warp.run port $ app dc)
    $ \_ -> cont DC
      { dcRouter = initRouterMVar
      , dcRuleIds = initRuleIds
      , dcRulesFile = mRulesFile
      , dcFileCache = initFileCache
      }

-- | Run a decoy server synchronously given a port and optional rules file.
--
-- @since 0.1.0.0
runDecoyServer :: Warp.Port -> Maybe FilePath -> IO ()
runDecoyServer port mRulesFile = do
  rules <- loadRulesFile mRulesFile
  initRouterMVar <- newMVar $ R.mkRouter rules
  initFileCache <- newMVar mempty
  initRuleIds <- newMVar mempty
  let dc = DC
        { dcRouter = initRouterMVar
        , dcRuleIds = initRuleIds
        , dcRulesFile = mRulesFile
        , dcFileCache = initFileCache
        }
  Warp.run port $ app dc

-- | Add a new rule to running decoy server.
--
-- @since 0.1.0.0
addRule :: DecoyCtx -> Rule -> IO RuleId
addRule dc rule = do
  rId <- newRuleId dc rule
  modifyMVar (dcRouter dc) $ \router ->
    pure ( R.addRouterRule rule {ruleId = rId} router
         , rId
         )

newRuleId :: DecoyCtx -> Rule -> IO RuleId
newRuleId dc rule = do
  modifyMVar (dcRuleIds dc) $ \rIds -> do
    let newId = maybe initRuleId (succ . fst) (M.lookupMax rIds)
    pure ( M.insert newId (reqPath (request rule)) rIds
         , newId
         )

addRules :: DecoyCtx -> [Rule] -> IO [RuleId]
addRules dc = traverse (addRule dc)

-- | Remove a rule from a decoy server.
--
-- @since 0.1.0.0
removeRule :: DecoyCtx -> RuleId -> IO ()
removeRule dc rId = do
  rIds <- readMVar $ dcRuleIds dc
  let mPath = M.lookup rId rIds
  for_ mPath $ \path ->
    modifyMVar_ (dcRouter dc) $ pure . R.removeRouterRule rId path
  modifyMVar_ (dcRuleIds dc) $ pure . M.delete rId

removeRules :: DecoyCtx -> [RuleId] -> IO ()
removeRules dc = traverse_ (removeRule dc)

-- | Run an action with a given rule added and remove it afterwards
--
-- @since 0.1.0.0
withRule :: DecoyCtx -> Rule -> IO a -> IO a
withRule dc rule action =
  bracket
    (addRule dc rule)
    (removeRule dc)
    (const action)

-- | Run an action with the given rules added and remove them afterwards
--
-- @since 0.1.0.0
withRules :: DecoyCtx -> [Rule] -> IO a -> IO a
withRules dc rules action =
  bracket
    (addRules dc rules)
    (removeRules dc)
    (const action)

-- | Resets a running server to its initial state.
--
-- @since 0.1.0.0
reset :: DecoyCtx -> IO ()
reset dc = do
  putMVar (dcRouter dc) . R.mkRouter
    =<< loadRulesFile (dcRulesFile dc)
  putMVar (dcRuleIds dc) mempty

loadRulesFile :: Maybe FilePath -> IO [RuleWithId]
loadRulesFile Nothing = pure []
loadRulesFile (Just rulesFile) = do
  rulesFileExists <- Dir.doesPathExist rulesFile
  if rulesFileExists
     then do
       values <- Aeson.eitherDecodeFileStrict rulesFile
       case traverse compileRule =<< values of
         Left err -> fail $ "Failed to parse rules file: " <> err
         Right rules -> pure $ zipWith (\r i -> r {ruleId = i}) rules [initRuleId..]
     else fail $ "File does not exist: " <> rulesFile

app :: DecoyCtx -> Wai.Application
app dc req respHandler = do
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
    ["_add-rule"] ->
      case compileRule
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rule -> do
          rId <- addRule dc rule
          respHandler $ jsonResponse rId

    ["_add-rules"] ->
      case traverse compileRule
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rules -> do
          rIds <- addRules dc rules
          respHandler $ jsonResponse rIds

    ["_reset-rules"] -> do
      putMVar (dcRouter dc) . R.mkRouter =<< loadRulesFile (dcRulesFile dc)
      respHandler $ Wai.responseLBS Http.ok200 [] "Rules reset"

    _ -> do
      router <- readMVar $ dcRouter dc
      respHandler =<<
        case eReqBodyJson of
          Left err -> pure . Wai.responseLBS Http.badRequest400 []
                        $ "Invalid JSON: " <> BS8.pack err
          Right mReqJson ->
            case R.matchEndpoint queryMap reqBodyBS mReqJson reqHeaders reqMethod router reqPath of
              Nothing -> pure $ Wai.responseLBS Http.notFound404 [] "No rule matched"
              Just matched ->
                handleMatchedEndpoint queryMap mReqJson (dcFileCache dc) matched

handleMatchedEndpoint
  :: R.QueryParams
  -> Maybe Aeson.Value
  -> MVar FileCache
  -> R.MatchedEndpoint
  -> IO Wai.Response
handleMatchedEndpoint queryParams mReqJson fileCacheMVar
          R.MkMatchedEndpoint{ R.responseBody, R.contentType, R.statusCode, R.pathParams } =
  case responseBody of
    Template resp -> pure $
      Wai.responseLBS respCode
        respHeaders
        (LBS.fromStrict $ TE.encodeUtf8 resp)
    File file -> do
      eContent <- getFileCache fileCacheMVar file
      case eContent of
        Left errResp -> pure errResp
        Right content -> do
          let rendered = LBS.fromStrict . TE.encodeUtf8
                       $ R.renderTemplate pathParams queryParams mReqJson content
          pure $ Wai.responseLBS respCode respHeaders rendered
    NoBody -> pure $ Wai.responseLBS respCode respHeaders ""
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

-- | Get template file contents from cache or read from disk and add to cache
getFileCache
  :: MVar FileCache
  -> FilePath
  -> IO (Either Wai.Response Stache.Template)
getFileCache fileCacheMVar file = do
  fileCache <- readMVar fileCacheMVar
  case M.lookup file fileCache of
    Nothing -> do
      exists <- Dir.doesFileExist file
      if not exists
      then pure .
        Left . Wai.responseLBS Http.notFound404 []
          $ "File not found: " <> BS8.pack file
      else do
        eContent <- Stache.compileTemplate "" <$> T.readFile file
        case eContent of
          Left err ->
            pure . Left . Wai.responseLBS Http.status500 []
              $ "Invalid mustache template: " <> BS8.pack (show err)
          Right content -> do
            modifyMVar_ fileCacheMVar $ pure . M.insert file content
            pure $ Right content
    Just cached -> pure $ Right cached

jsonResponse
  :: Aeson.ToJSON body
  => body
  -> Wai.Response
jsonResponse body =
  Wai.responseLBS Http.ok200 [(Http.hContentType, "application/json")]
    $ Aeson.encode body
