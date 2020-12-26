{-# LANGUAGE BlockArguments #-}
module Kassandra.Standalone.State
  ( localBackendProvider, taskMonitor
  ) where

import           Control.Concurrent.Async       ( cancel )
import           Control.Concurrent.STM         ( TQueue
                                                , readTQueue
                                                )
import           Control.Monad                  ( foldM )
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import           Kassandra.Api                  ( SocketMessage(TaskUpdates)
                                                , SocketRequest
                                                  ( AllTasks
                                                  , ChangeTasks
                                                  , UIConfigRequest
                                                  )
                                                )
import           Kassandra.Config               ( LocalBackend )
import           Kassandra.LocalBackend         ( LocalBackendRequest
                                                , LocalBackendResponse
                                                  ( DataResponse
                                                  , SetupComplete
                                                  )
                                                )
import           Kassandra.State                ( StateProvider
                                                , TaskProvider
                                                )
import           Kassandra.Types         hiding ( getTasks )
import qualified Network.Simple.TCP            as Net
import qualified Reflex                        as R
import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )

localBackendProvider :: TQueue LocalBackendRequest -> IO ()
localBackendProvider requests = go Nothing
 where
  go request = go =<< listenWithBackgroundThreads request
  makeRequest = atomically $ readTQueue requests
  listenWithBackgroundThreads (Just (config, callback, socketRequests)) =
    let cb      = callback . DataResponse . TaskUpdates
        monitor = taskMonitor config cb
        handler = requestsHandler config socketRequests cb
    in  withAsync (concurrently_ monitor handler) $ const do
          callback SetupComplete
          makeRequest
  listenWithBackgroundThreads _ = makeRequest

requestsHandler
  :: LocalBackend -> TQueue SocketRequest -> (NonEmpty Task -> IO ()) -> IO ()
requestsHandler backend requests callback = forever $ do
  socketRequest <- atomically $ readTQueue requests
  case socketRequest of
    UIConfigRequest   -> error "LocalBackend is not supposed to ask for Config"
    AllTasks          -> whenNotNullM (getTasks []) callback
    ChangeTasks tasks -> saveTasks $ toList tasks

taskMonitor :: LocalBackend -> (NonEmpty Task -> IO ()) -> IO ()
taskMonitor backend newTasksCallBack = do
  putStrLn "Listening for changed or new tasks on 127.0.0.1:6545."
  Net.serve (Net.Host "127.0.0.1") "6545" $ \(socket, _) ->
    Net.recv socket 4096 >>= maybe
      (putStrLn "Unsuccessful connection attempt.")
      (\changes ->
        either
            (\err -> putStrLn [i|Couldn‘t decode #{changes} as Task: #{err}|])
            (newTasksCallBack . one)
          $ Aeson.eitherDecodeStrict @Task changes
      )

   {-
ioTaskProvider
  :: (WidgetIO t m) => MVar (NonEmpty Task -> IO ()) -> TaskProvider t m
ioTaskProvider callbackSlot changeTaskEvent = do
  void . R.performEvent $ liftIO .  <$> changeTaskEvent
  (tasksEvent, newTasksCallBack) <- R.newTriggerEvent
  tasks <- R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
                     HashMap.empty
                     (changeTaskEvent <> tasksEvent)
  putMVar callbackSlot newTasksCallBack
  R.holdUniqDyn tasks

ioStateFeeder :: MVar (NonEmpty Task -> IO ()) -> IO ()
ioStateFeeder callbackSlot = do
  callback <- takeMVar callbackSlot
  concurrently_ () (taskMonitor callback)

ioStateProvider
  :: WidgetIO t m => MVar (NonEmpty Task -> IO ()) -> StateProvider t m
ioStateProvider callbackSlot = stateProvider (ioTaskProvider callbackSlot) -}
