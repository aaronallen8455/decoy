{-# LANGUAGE OverloadedStrings #-}
module Decoy
  ( -- * Starting a server instance
    withDecoyServer
  , runDecoyServer
  , decoyServerAsync
  , runDecoyServerFromCmdLine
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
  , LoggingOpt(..)
  , R.Router
  , module Rule
  , module Mod
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar
import           Control.Exception (bracket)
import           Control.Monad (foldM, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import           Data.Traversable (for)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as OP
import           Options.Applicative.Help.Pretty ((.$.), Doc)
import qualified System.Directory as Dir
import qualified System.Log.FastLogger as Log
import qualified System.Log.FastLogger.Date as Date
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
  , dcFileCache :: MVar FileCache
  , dcModifiers :: MVar (M.Map ModifierId ModifierWithId)
  , dcRulesFiles :: [FilePath]
  }

data Config = MkConfig
  { stdoutLogger :: Maybe Log.TimedFastLogger
  , stderrLogger :: Maybe Log.TimedFastLogger
  , quiet :: Bool
  , verbose :: Bool
  , port :: Warp.Port
  }

withConfig :: Warp.Port -> LoggingOpt -> (Config -> IO a) -> IO a
withConfig port logOpt cont =
  case logOpt of
    NoLogging -> cont MkConfig
      { stdoutLogger = Nothing
      , stderrLogger = Nothing
      , quiet = True
      , verbose = False
      , port = port
      }
    Logging -> do
      timeCache <- Date.newTimeCache Date.simpleTimeFormat
      Log.withTimedFastLogger timeCache (Log.LogStdout Log.defaultBufSize) $ \stdoutLogger ->
        Log.withTimedFastLogger timeCache (Log.LogStderr Log.defaultBufSize) $ \stderrLogger ->
          cont MkConfig
            { stdoutLogger = Just stdoutLogger
            , stderrLogger = Just stderrLogger
            , quiet = False
            , verbose = False
            , port = port
            }
    VerboseLogging -> do
      timeCache <- Date.newTimeCache Date.simpleTimeFormat
      Log.withTimedFastLogger timeCache (Log.LogStdout Log.defaultBufSize) $ \stdoutLogger ->
        Log.withTimedFastLogger timeCache (Log.LogStderr Log.defaultBufSize) $ \stderrLogger ->
          cont MkConfig
            { stdoutLogger = Just stdoutLogger
            , stderrLogger = Just stderrLogger
            , quiet = False
            , verbose = True
            , port = port
            }

type FileCache = M.Map FilePath Stache.Template

-- | Indicates what mode of logging to use
--
-- @since 0.1.0.0
data LoggingOpt
  = NoLogging -- ^ Suppress all logging
  | Logging -- ^ Print default logging messages to stdout and stderr
  | VerboseLogging -- ^ Print verbose logging messages to stdout and stderr

-- | Run a decoy server in a child thread given a port and optional rules files.
--
-- __Example:__
--
-- @
-- withDecoyServer 8080 [] NoLogging $ \dc -> do
--    addRules dc someRules
--    _ <- httpBS "GET http://localhost:8080/some/path"
-- @
--
-- @since 0.1.0.0
withDecoyServer
  :: Warp.Port
  -> [FilePath]
  -> LoggingOpt
  -> (DecoyCtx -> IO a)
  -> IO a
withDecoyServer port rulesFiles loggingOpt cont = do
  dc <- initDecoyCtx rulesFiles
  withConfig port loggingOpt $ \cfg ->
    Async.withAsync (Warp.run port $ app cfg dc)
      $ \_ -> cont dc

-- | Run a decoy server in the returned @Async@. The caller is responsible for
-- canceling the async.
decoyServerAsync
  :: Warp.Port
  -> [FilePath]
  -> LoggingOpt
  -> IO (DecoyCtx, Async.Async ())
decoyServerAsync port rulesFiles loggingOpt = do
  dc <- initDecoyCtx rulesFiles
  async <- Async.async $
    withConfig port loggingOpt $ \cfg ->
      Warp.run port $ app cfg dc
  pure (dc, async)

-- | Run a decoy server synchronously given a port and optional rules files.
--
-- @since 0.1.0.0
runDecoyServer :: Warp.Port -> [FilePath] -> LoggingOpt -> IO ()
runDecoyServer port rulesFiles loggingOpt = do
  dc <- initDecoyCtx rulesFiles
  withConfig port loggingOpt $ \cfg ->
    Warp.run port $ app cfg dc

-- | Initializes the server from command line arguments.
--
-- @since 0.1.0.0
runDecoyServerFromCmdLine :: IO ()
runDecoyServerFromCmdLine = do
  timeCache <- Date.newTimeCache Date.simpleTimeFormat
  Log.withTimedFastLogger timeCache (Log.LogStdout Log.defaultBufSize) $ \stdoutLogger ->
    Log.withTimedFastLogger timeCache (Log.LogStderr Log.defaultBufSize) $ \stderrLogger -> do
      let configParser =
            MkConfig
              (Just stdoutLogger)
              (Just stderrLogger)
              <$> OP.switch
                  ( OP.long "quiet"
                 <> OP.short 'q'
                 <> OP.help "Suppress all logging"
                  )
              <*> OP.switch
                  ( OP.long "verbose"
                 <> OP.short 'v'
                 <> OP.help "Log all requests and responses"
                  )
              <*> OP.option OP.auto
                  ( OP.long "port"
                 <> OP.short 'p'
                 <> OP.help "Network port the server will bind to"
                 <> OP.showDefault
                 <> OP.value 9000
                 <> OP.metavar "PORT"
                  )
          rulesFilesParser =
            OP.option (words <$> OP.str)
              ( OP.long "rules-files"
             <> OP.short 'f'
             <> OP.help "A list of paths to files containing rules to load on startup, whitespace separated"
             <> OP.value []
             <> OP.metavar "FILES"
              )
          opts = OP.info (OP.helper <*> ((,) <$> configParser <*> rulesFilesParser))
            ( OP.fullDesc
            <> OP.progDesc "Highly customizable mock data server"
            <> OP.headerDoc (Just logo)
            )
      (cfg, rulesFiles) <- OP.execParser opts
      dc <- initDecoyCtx rulesFiles
      logInfo cfg $ "Decoy starting on port " <> Log.toLogStr (show (port cfg))
      Warp.run (port cfg) $ app cfg dc

logo :: Doc
logo = " decoy      .-."
   .$. "    ,      ( {o\\"
   .$. "    {`\"=,___) (`~"
   .$. "     \\  ,_.-   )"
   .$. "~^~^~^`- ~^ ~^ '~^~^~^~"

initDecoyCtx :: [FilePath] -> IO DecoyCtx
initDecoyCtx rulesFiles = do
  rules <- loadRulesFiles rulesFiles
  initRouterMVar <- newMVar R.emptyRouter
  initFileCache <- newMVar mempty
  initRuleIds <- newMVar mempty
  initModifierMVar <- newMVar mempty
  let dc = DC
        { dcRouter = initRouterMVar
        , dcRuleIds = initRuleIds
        , dcRulesFiles = rulesFiles
        , dcFileCache = initFileCache
        , dcModifiers = initModifierMVar
        }
  void $ addRules dc rules
  pure dc

-- | Add new rules to running decoy server.
--
-- @since 0.1.0.0
addRules :: DecoyCtx -> [Rule] -> IO [RuleWithId]
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

  pure rules

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
    (map ruleId <$> addRules dc rules)
    (removeRules dc)
    (const action)

-- | Add new modifiers to running decoy server.
--
-- @since 0.1.0.0
addModifiers :: DecoyCtx -> [Modifier] -> IO [ModifierWithId]
addModifiers dc modifiers = do
  modifyMVar (dcModifiers dc) $ \mods -> do
    let nextModId = maybe initModifierId (succ . fst) $ M.lookupMax mods
        newModifiers =
          zipWith (\i m -> (i, m { modifierId = i })) [nextModId ..] modifiers
    pure ( mods <> M.fromList newModifiers
         , snd <$> newModifiers
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
    (map modifierId <$> addModifiers dc rules)
    (removeModifiers dc)
    (const action)

-- | Resets a running server to its initial state.
--
-- @since 0.1.0.0
reset :: DecoyCtx -> IO ()
reset dc = do
  putMVar (dcRuleIds dc) mempty
  putMVar (dcModifiers dc) mempty
  putMVar (dcRouter dc) R.emptyRouter
  void $ addRules dc =<< loadRulesFiles (dcRulesFiles dc)

loadRulesFiles :: [FilePath] -> IO [Rule]
loadRulesFiles rulesFiles =
  fmap concat . for rulesFiles $ \rulesFile -> do
    rulesFileExists <- Dir.doesPathExist rulesFile
    if rulesFileExists
       then do
         values <- Aeson.eitherDecodeFileStrict rulesFile
         case traverse compileRule =<< values of
           Left err -> fail $ "Failed to parse rules file: " <> rulesFile <> ": " <> err
           Right rules -> pure rules
       else fail $ "File does not exist: " <> rulesFile

app :: Config -> DecoyCtx -> Wai.Application
app cfg dc req respHandler = do
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
        Left err -> do
          logVerboseErr cfg $ "_add-rules: Failed to parse request body: " <> Log.toLogStr err
          respHandler
            . Wai.responseLBS Http.badRequest400 []
            $ "Invalid JSON: " <> BS8.pack err
        Right rules -> do
          newRules <- addRules dc rules
          logVerboseInfo cfg $ "Added modifiers: " <> Log.toLogStr (show newRules)
          respHandler $ jsonResponse (ruleId <$> newRules)

    ["_remove-rules"] ->
      case Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> do
          logVerboseErr cfg $ "_remove-rules: Failed to parse request body: " <> Log.toLogStr err
          respHandler
            . Wai.responseLBS Http.badRequest400 []
            $ "Invalid JSON: " <> BS8.pack err
        Right rIds -> do
          removeRules dc rIds
          logVerboseInfo cfg $ "Removed rules: "
                            <> Log.toLogStr (List.intercalate ", " (show <$> rIds))
          respHandler $ Wai.responseLBS Http.ok200 [] "Rules removed"

    ["_add-modifiers"] ->
      case traverse compileModifier
             =<< Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> do
          logVerboseErr cfg $ "_add-modifiers: Failed to parse request body: " <> Log.toLogStr err
          respHandler
            . Wai.responseLBS Http.badRequest400 []
            $ "Invalid JSON: " <> BS8.pack err
        Right modifiers -> do
          newMods <- addModifiers dc modifiers
          logVerboseInfo cfg $ "Added modifiers: " <> Log.toLogStr (show newMods)
          respHandler $ jsonResponse (modifierId <$> newMods)

    ["_remove-modifiers"] ->
      case Aeson.parseEither Aeson.parseJSON
             =<< maybe (Left "No body") Right
             =<< eReqBodyJson of
        Left err -> do
          logVerboseErr cfg $ "_remove-modifiers: Failed to parse request body: " <> Log.toLogStr err
          respHandler
            . Wai.responseLBS Http.badRequest400 []
            $ "Invalid JSON: " <> BS8.pack err
        Right modIds -> do
          removeModifiers dc modIds
          logVerboseInfo cfg $ "Removed modifiers: "
                            <> Log.toLogStr (List.intercalate ", " (show <$> modIds))
          respHandler $ Wai.responseLBS Http.ok200 [] "Modifiers removed"

    ["_reset"] -> do
      putMVar (dcRouter dc) R.emptyRouter
      putMVar (dcModifiers dc) mempty
      putMVar (dcRuleIds dc) mempty
      void $ addRules dc =<< loadRulesFiles (dcRulesFiles dc)
      logVerboseInfo cfg "Server reset"
      respHandler $ Wai.responseLBS Http.ok200 [] "server reset"

    _ -> do
      router <- readMVar $ dcRouter dc
      respHandler =<<
        case eReqBodyJson of
          Left err -> pure . Wai.responseLBS Http.badRequest400 []
                        $ "Invalid JSON: " <> BS8.pack err
          Right mReqJson ->
            case R.matchEndpoint queryMap reqBodyBS mReqJson reqHeaders reqMethod router reqPath of
              Nothing -> do
                logErr cfg $ "No rule matched: " <>
                  if verbose cfg
                     then Log.toLogStr (show req)
                     else Log.toLogStr (Wai.rawPathInfo req)
                pure $ Wai.responseLBS Http.notFound404 [] "No rule matched"
              Just matched -> do
                modifiers <- readMVar (dcModifiers dc)
                let matchedModifiers =
                      filter
                        (matchModifier (R.matchedRuleId matched) queryMap reqBodyBS mReqJson reqHeaders reqMethod reqPath)
                        (M.elems modifiers)
                handleMatchedEndpoint
                  cfg
                  (logMatchedEndpoint cfg reqMethod (Wai.rawPathInfo req) (R.statusCode matched))
                  queryMap
                  mReqJson
                  (dcFileCache dc)
                  matched
                  matchedModifiers

logInfo :: Config -> Log.LogStr -> IO ()
logInfo cfg msg =
  unless (quiet cfg)
    $ traverse_ (applyLogger msg) (stdoutLogger cfg)

logVerboseInfo :: Config -> Log.LogStr -> IO ()
logVerboseInfo cfg msg =
  when (not (quiet cfg) && verbose cfg)
    $ traverse_ (applyLogger msg) (stdoutLogger cfg)

logErr :: Config -> Log.LogStr -> IO ()
logErr cfg msg =
  unless (quiet cfg)
    $ traverse_ (applyLogger msg) (stderrLogger cfg)

logVerboseErr :: Config -> Log.LogStr -> IO ()
logVerboseErr cfg msg =
  when (not (quiet cfg) && verbose cfg)
    $ traverse_ (applyLogger msg) (stderrLogger cfg)

applyLogger :: Log.LogStr -> Log.TimedFastLogger -> IO ()
applyLogger msg logger =
  logger (\time -> Log.toLogStr time <> "| " <> msg <> "\n")

logMatchedEndpoint :: Config -> Http.Method -> BS.ByteString -> Maybe Word -> Maybe LBS.ByteString -> IO ()
logMatchedEndpoint cfg reqMethod reqPath mStatusCode mRespBody =
  logInfo cfg $ maybe "200" Log.toLogStr mStatusCode
             <> " - [" <> Log.toLogStr reqMethod <> "] "
             <> Log.toLogStr reqPath
             <> if verbose cfg -- include response body if verbose
                then foldMap (\x -> ": " <> Log.toLogStr x) mRespBody
                else mempty

handleMatchedEndpoint
  :: Config
  -> (Maybe LBS.ByteString -> IO ())
  -> R.QueryParams
  -> Maybe Aeson.Value
  -> MVar FileCache
  -> R.MatchedEndpoint
  -> [ModifierWithId]
  -> IO Wai.Response
handleMatchedEndpoint cfg logSuccess queryParams mReqJson fileCacheMVar
          R.MkMatchedEndpoint{ R.responseBody, R.contentType, R.statusCode, R.pathParams }
          matchedModifiers = do
  let respHeaders =
        [ (Http.hContentType, TE.encodeUtf8 ct) | Just ct <- [contentType] ]
      respCode = case statusCode of
        Nothing -> Http.ok200
        Just sc ->
          case M.lookup (fromIntegral sc) allHttpStatuses of
            Nothing -> Http.mkStatus (fromIntegral sc) mempty
            Just c -> c

  case responseBody of
    Template resp -> do
      let r = applyModifierPatches matchedModifiers
            . LBS.fromStrict
            $ TE.encodeUtf8 resp
      logSuccess (Just r)
      pure $ Wai.responseLBS respCode respHeaders r
    File file -> do
      eContent <- getFileCache fileCacheMVar file
      case eContent of
        Left (FileNotFound fileName) -> do
          let r = "File not found: " <> BS8.pack fileName
          logErr cfg $ "Failed to render response: " <> Log.toLogStr r
          pure $ Wai.responseLBS Http.notFound404 [] r
        Left (InvalidMustacheTemplate err) -> do
          let r = "Invalid mustache template: " <> BS8.pack (show err)
          logErr cfg $ "Failed to render response: " <> Log.toLogStr r
          pure $ Wai.responseLBS Http.status500 [] r
        Right content -> do
          let r = applyModifierPatches matchedModifiers
                . LBS.fromStrict . TE.encodeUtf8
                $ R.renderTemplate pathParams queryParams mReqJson content
          logSuccess (Just r)
          pure $ Wai.responseLBS respCode respHeaders r
    NoBody -> do
      logSuccess Nothing
      pure $ Wai.responseLBS respCode respHeaders ""

allHttpStatuses :: M.Map Int Http.Status
allHttpStatuses = M.fromList $ (\x -> (Http.statusCode x, x)) <$> [minBound .. maxBound]

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
    ByRequest req ->
      R.matchPath (reqPath req) path
      &&
      R.matchRequest queryMap reqBodyBS mReqJson reqHeaders reqMethod req

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
  -> IO (Either RenderingError Stache.Template)
getFileCache fileCacheMVar file = do
  fileCache <- readMVar fileCacheMVar
  case M.lookup file fileCache of
    Nothing -> do
      exists <- Dir.doesFileExist file
      if not exists
      then pure . Left $ FileNotFound file
      else do
        eContent <- Stache.compileTemplate "" <$> T.readFile file
        case eContent of
          Left err -> pure . Left $ InvalidMustacheTemplate (show err)
          Right content -> do
            modifyMVar_ fileCacheMVar $ pure . M.insert file content
            pure $ Right content
    Just cached -> pure $ Right cached

data RenderingError
  = FileNotFound FilePath
  | InvalidMustacheTemplate String

jsonResponse
  :: Aeson.ToJSON body
  => body
  -> Wai.Response
jsonResponse body =
  Wai.responseLBS Http.ok200 [(Http.hContentType, "application/json")]
    $ Aeson.encode body
