module Kassandra.Standalone.State
  ( localBackendProvider
  )
where

import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )
import qualified Data.HashMap.Strict           as HashMap
import qualified Reflex                        as R
import           Kassandra.Types         hiding ( getTasks )
import           Kassandra.State                ( stateProvider
                                                , TaskProvider
                                                , StateProvider
                                                )
import           Kassandra.Config               ( LocalBackend )
import           Kassandra.LocalBackend         ( LocalBackendRequest
                                                , LocalBackendResponse
                                                  ( DataResponse
                                                  )
                                                )
import           Kassandra.Api                  ( SocketRequest
                                                  ( UIConfigRequest
                                                  , AllTasks
                                                  , ChangeTasks
                                                  )
                                                , SocketMessage(TaskUpdates)
                                                )
import           Control.Concurrent.STM         ( TQueue
                                                , readTQueue
                                                )
import           Control.Monad                  ( foldM )
import           Control.Concurrent.Async       ( cancel )
import qualified Network.Simple.TCP            as Net
import qualified Data.Aeson                    as Aeson

localBackendProvider :: TQueue LocalBackendRequest -> IO ()
localBackendProvider requests = go Nothing
 where
  go request = go =<< listenWithBackgroundThreads request
  makeRequest = atomically $ readTQueue requests
  listenWithBackgroundThreads (Just (config, callback, socketRequests)) =
    let cb      = callback . DataResponse . TaskUpdates
        monitor = taskMonitor config cb
        handler = requestsHandler config socketRequests cb
    in  withAsync (concurrently_ monitor handler) $ const makeRequest
  listenWithBackgroundThreads Nothing = makeRequest

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
