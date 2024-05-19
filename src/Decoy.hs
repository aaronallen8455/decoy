{-# LANGUAGE OverloadedStrings #-}
module Decoy
  ( -- * Starting a server instance
    withDecoyServer
  , runDecoyServer
  , decoyServerAsync
    -- * Modify a running instance
  , addRules
  , removeRules
  , withRules
  , addModifiers
  , removeModifiers
  , withModifiers
  , reset
    -- * Types
  , DecoyCtx(..)
  , R.Router
  , module Rule
  , module Mod
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar
import           Control.Exception (bracket)
import           Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as Dir
import qualified Text.Mustache as Stache

import           Decoy.Modifier as Mod
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
  , dcModifiers :: MVar (M.Map ModifierId ModifierWithId)
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
  dc <- initDecoyCtx mRulesFile rules
  Async.withAsync (Warp.run port $ app dc)
    $ \_ -> cont dc

-- | Run a decoy server in the returned @Async@. The caller is responsible for
-- canceling the async.
decoyServerAsync :: Warp.Port -> Maybe FilePath -> IO (DecoyCtx, Async.Async ())
decoyServerAsync port mRulesFile = do
  rules <- loadRulesFile mRulesFile
  dc <- initDecoyCtx mRulesFile rules
  async <- Async.async (Warp.run port $ app dc)
  pure (dc, async)

-- | Run a decoy server synchronously given a port and optional rules file.
--
-- @since 0.1.0.0
runDecoyServer :: Warp.Port -> Maybe FilePath -> IO ()
runDecoyServer port mRulesFile = do
  rules <- loadRulesFile mRulesFile
  dc <- initDecoyCtx mRulesFile rules
  Warp.run port $ app dc

initDecoyCtx :: Maybe FilePath -> [RuleWithId] -> IO DecoyCtx
initDecoyCtx mRulesFile rules = do
  initRouterMVar <- newMVar $ R.mkRouter rules
  initFileCache <- newMVar mempty
  initRuleIds <- newMVar mempty
  initModifierMVar <- newMVar mempty
  pure DC
    { dcRouter = initRouterMVar
    , dcRuleIds = initRuleIds
    , dcRulesFile = mRulesFile
    , dcFileCache = initFileCache
    , dcModifiers = initModifierMVar
    }

-- | Add new rules to running decoy server.
--
-- @since 0.1.0.0
addRules :: DecoyCtx -> [Rule] -> IO [RuleId]
addRules dc ruleSpecs = do
  rules <- modifyMVar (dcRuleIds dc) $ \rIds -> do
    let nextId = maybe initRuleId (succ . fst) (M.lookupMax rIds)
        rules = zipWith (\i r -> (i, r { ruleId = i })) [nextId ..] ruleSpecs
        pathsWithIds = fmap (fmap (reqPath . request)) rules
    pure ( rIds <> M.fromList pathsWithIds
         , snd <$> rules
         )

  modifyMVar_ (dcRouter dc) $ \router ->
    pure $ R.addRouterRules rules router

  pure $ ruleId <$> rules

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

-- | Remove given rules from a decoy server.
--
-- @since 0.1.0.0
removeRules :: DecoyCtx -> [RuleId] -> IO ()
removeRules dc = traverse_ (removeRule dc)

-- | Run an action with the given rules added and remove them afterwards
--
-- @since 0.1.0.0
withRules :: DecoyCtx -> [Rule] -> IO a -> IO a
withRules dc rules action =
  bracket
    (addRules dc rules)
    (removeRules dc)
    (const action)

-- | Add new modifiers to running decoy server.
--
-- @since 0.1.0.0
addModifiers :: DecoyCtx -> [Modifier] -> IO [ModifierId]
addModifiers dc modifiers = do
  modifyMVar (dcModifiers dc) $ \mods -> do
    let nextModId = maybe initModifierId (succ . fst) $ M.lookupMax mods
        newModifiers =
          zipWith (\i m -> (i, m { modifierId = i })) [nextModId ..] modifiers
    pure ( mods <> M.fromList newModifiers
         , fst <$> newModifiers
         )

-- | Remove given modifiers from a decoy server.
--
-- @since 0.1.0.0
removeModifiers :: DecoyCtx -> [ModifierId] -> IO ()
removeModifiers dc modIds =
  modifyMVar_ (dcModifiers dc) $ \mods ->
    pure $ foldl' (flip M.delete) mods modIds

-- | Run an action with the given modifiers added and remove them afterwards
--
-- @since 0.1.0.0
withModifiers :: DecoyCtx -> [Modifier] -> IO a -> IO a
withModifiers dc rules action =
  bracket
    (addModifiers dc rules)
    (removeModifiers dc)
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
  let removeTrailingEmpty = reverse . dropEmpty . reverse where
        dropEmpty ("" : rest) = rest
        dropEmpty x = x
      reqPath = removeTrailingEmpty $ Wai.pathInfo req
      queryMap = M.mapKeys TE.decodeUtf8Lenient
               . fmap (fmap TE.decodeUtf8Lenient)
               . M.fromList
               $ Wai.queryString req
      reqHeaders = Wai.requestHeaders req
      reqMethod = Wai.requestMethod req
  reqBodyBS <- Wai.strictRequestBody req
  let eReqBodyJson =
        case lookup Http.hContentType reqHeaders of
          Just ct | "json" `BS.isInfixOf` ct && not (BS8.null reqBodyBS) ->
            Just <$> Aeson.eitherDecode reqBodyBS
          _ -> Right Nothing

  case reqPath of
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

    ["_remove-rules"] ->
      case Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right rIds -> do
          removeRules dc rIds
          respHandler $ Wai.responseLBS Http.ok200 [] "Rules removed"

    ["_add-modifiers"] ->
      case traverse compileModifier
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right modifiers -> do
          modIds <- addModifiers dc modifiers
          respHandler $ jsonResponse modIds

    ["_remove-modifiers"] ->
      case Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> respHandler
                  . Wai.responseLBS Http.badRequest400 []
                  $ "Invalid JSON: " <> BS8.pack err
        Right modIds -> do
          removeModifiers dc modIds
          respHandler $ Wai.responseLBS Http.ok200 [] "Modifiers removed"

    ["_reset"] -> do
      putMVar (dcRouter dc) . R.mkRouter =<< loadRulesFile (dcRulesFile dc)
      putMVar (dcModifiers dc) mempty
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
              Just matched -> do
                modifiers <- readMVar (dcModifiers dc)
                let matchedModifiers =
                      filter
                        (matchModifier (R.matchedRuleId matched) queryMap reqBodyBS mReqJson reqHeaders reqMethod reqPath)
                        (M.elems modifiers)
                handleMatchedEndpoint queryMap mReqJson (dcFileCache dc) matched matchedModifiers

handleMatchedEndpoint
  :: R.QueryParams
  -> Maybe Aeson.Value
  -> MVar FileCache
  -> R.MatchedEndpoint
  -> [ModifierWithId]
  -> IO Wai.Response
handleMatchedEndpoint queryParams mReqJson fileCacheMVar
          R.MkMatchedEndpoint{ R.responseBody, R.contentType, R.statusCode, R.pathParams }
          matchedModifiers = do
  case responseBody of
    Template resp -> pure $
      Wai.responseLBS respCode respHeaders
        . applyModifierPatches matchedModifiers
        . LBS.fromStrict
        $ TE.encodeUtf8 resp
    File file -> do
      eContent <- getFileCache fileCacheMVar file
      case eContent of
        Left errResp -> pure errResp
        Right content -> do
          let rendered = applyModifierPatches matchedModifiers
                       . LBS.fromStrict . TE.encodeUtf8
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

matchModifier
  :: RuleId
  -> R.QueryParams
  -> LBS.ByteString
  -> Maybe Aeson.Value
  -> Http.RequestHeaders
  -> Http.Method
  -> [T.Text]
  -> ModifierWithId
  -> Bool
matchModifier ruleId queryMap reqBodyBS mReqJson reqHeaders reqMethod path modifier =
  case matcher modifier of
    ById rId -> rId == ruleId
    ByRule rule ->
      R.matchPath (reqPath $ request rule) path
      &&
      R.matchRule queryMap reqBodyBS mReqJson reqHeaders reqMethod rule

applyModifierPatches :: [ModifierWithId] -> LBS.ByteString -> LBS.ByteString
applyModifierPatches [] bs = bs
applyModifierPatches modifiers bs =
  case Aeson.decode bs of
    Nothing -> bs
    Just val ->
      case foldM (flip AD.patch) val (jsonPatch <$> modifiers) of
        Aeson.Error _ -> bs
        Aeson.Success patched -> Aeson.encode patched

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
