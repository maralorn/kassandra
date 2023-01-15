{-# LANGUAGE BlockArguments #-}

module Kassandra.Standalone.State (
  localBackendProvider,
) where

import Control.Concurrent.STM (TQueue, readTQueue)
import Control.Concurrent.STM.TVar (stateTVar)
import Control.Monad.STM (retry)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Network.Simple.TCP as Net
import Say (say, sayErr)
import Streamly (
  SerialT,
  asyncly,
  maxThreads,
  parallel,
  parallely,
  serial,
 )
import qualified Streamly.Prelude as S
import Taskwarrior.IO (getTasks, saveTasks)

import Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
 )
import Kassandra.Backend.Calendar (
  Cache,
  getEvents,
  loadCache,
  newCache,
  saveCache,
  setList,
 )
import Kassandra.Config (LocalBackend, UserConfig (..))
import Kassandra.Debug (Severity (Debug), log)
import Kassandra.LocalBackend (
  LocalBackendRequest (LocalBackendRequest),
  alive,
  requestQueue,
  responseCallback,
  userConfig,
 )

foldToSeq :: Monad m => SerialT m a -> m (Seq a)
foldToSeq = S.foldl' (|>) mempty

waitTillFalse :: MonadIO m => TVar Bool -> m ()
waitTillFalse boolTvar = (atomically . whenM (readTVar boolTvar)) retry
concurrentWhileTrue :: TVar Bool -> IO a -> IO ()
concurrentWhileTrue boolTvar action = race_ action (waitTillFalse boolTvar)
lookupTMap :: (Ord k, MonadIO f) => k -> TVar (Map k a) -> f (Maybe a)
lookupTMap key tvarMap = Map.lookup key <$> readTVarIO tvarMap
insertOrAddTMap :: Ord k => k -> e -> TVar (Map k (Seq e)) -> STM Bool
insertOrAddTMap key entry tvarMap =
  stateTVar tvarMap \theMap ->
    second (\x -> Map.insert key x theMap) $
      Map.lookup key theMap & \case
        Just listeners -> (False, listeners |> entry)
        Nothing -> (True, one entry)

type ClientMap = Map LocalBackend (Seq (TVar Bool, SocketMessage -> IO ()))

localBackendProvider :: TQueue LocalBackendRequest -> IO ()
localBackendProvider requestQueue = newTVarIO mempty >>= handleRequests requestQueue

handleRequests :: TQueue LocalBackendRequest -> TVar ClientMap -> IO ()
handleRequests requestQueue mapVar = do
  cache <- newCache
  let go = atomically (readTQueue requestQueue) >>= \req -> concurrently_ (handleRequest req cache mapVar) go
  S.drain $
    ( liftIO (loadCache cache)
        `serial` (asyncly . maxThreads 100 . void . getEvents) cache
        `serial` liftIO (saveCache cache)
    )
      `parallel` liftIO go

monitorCallback :: LocalBackend -> TVar ClientMap -> NonEmpty Task -> IO ()
monitorCallback key mapVar tasks =
  whenJustM (lookupTMap key mapVar) $
    mapM_ (($ TaskUpdates (NESeq.fromList tasks)) . snd)

handleRequest :: LocalBackendRequest -> Cache -> TVar ClientMap -> IO ()
handleRequest req cache mapVar =
  S.drain
    . parallely
    . S.fromFoldableM
    $ [ do handleRequestsWhileAlive req cache; removeClientFromMap req mapVar
      , launchOrAttachMonitor req mapVar
      , responseCallback req ConnectionEstablished
      , say "Client registered on backend"
      ]

removeClientFromMap :: MonadIO m => LocalBackendRequest -> TVar ClientMap -> m ()
removeClientFromMap req mapVar = atomically (modifyTVar' mapVar updateMap)
 where
  key = localBackend . userConfig $ req
  filterClients = Seq.filter ((/= alive req) . fst)
  setEntry newEntry clientMap
    | null newEntry = Map.delete key clientMap
    | otherwise = Map.insert key newEntry clientMap
  updateMap clientMap =
    Map.lookup key clientMap & \case
      Just clients -> setEntry (filterClients clients) clientMap
      Nothing -> clientMap

launchOrAttachMonitor :: LocalBackendRequest -> TVar ClientMap -> IO ()
launchOrAttachMonitor LocalBackendRequest{userConfig, alive, responseCallback} mapVar =
  whenM (atomically $ insertOrAddTMap localBackend entry mapVar) $
    do
      whileClientsNotEmpty (taskMonitor localBackend (monitorCallback localBackend mapVar))
      say "Stopped listening for changes"
 where
  UserConfig{localBackend} = userConfig
  entry = (alive, responseCallback)
  waitForClientsEmpty = atomically . whenJustM (Map.lookup localBackend <$> readTVar mapVar) . const $ retry
  whileClientsNotEmpty action = race_ action waitForClientsEmpty

-- TODO: Use backend config

handleRequestsWhileAlive :: LocalBackendRequest -> Cache -> IO ()
handleRequestsWhileAlive LocalBackendRequest{userConfig, alive, responseCallback, requestQueue} cache =
  concurrentWhileTrue alive go
 where
  handler = \case
    UIConfigRequest -> (responseCallback . UIConfigResponse . uiConfig) userConfig
    AllTasks -> whenNotNullM (getTasks []) (responseCallback . TaskUpdates . NESeq.fromList)
    ChangeTasks tasks -> (saveTasks . toList) tasks
    SetCalendarList uid list -> do
      setList cache uid list
      sendCalendarEvents
    CalenderRequest -> sendCalendarEvents
  go = do
    nextRequest <- atomically (readTQueue requestQueue)
    concurrently_ (handler nextRequest) go
  sendCalendarEvents = do
    events <- foldToSeq (getEvents cache)
    log Debug [i|Sending #{Seq.length events} events|]
    responseCallback . CalendarEvents $ events

taskMonitor :: LocalBackend -> (NonEmpty Task -> IO ()) -> IO ()
taskMonitor _ newTasksCallBack = do
  say "Listening for changed or new tasks on 127.0.0.1:6545."
  Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) -> Net.recv socket 4096 >>= unwrapChanges
 where
  unwrapChanges = maybe (sayErr "Unsuccessful connection attempt.") handleChanges
  handleChanges changes =
    either
      (\err -> sayErr [i|Couldn‘t decode #{changes} as Task: #{err}|])
      (newTasksCallBack . one)
      . Aeson.eitherDecodeStrict @Task
      $ changes
