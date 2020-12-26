{-# LANGUAGE BlockArguments #-}
module Kassandra.Standalone.State
  ( localBackendProvider
  , taskMonitor
  ) where

import           Control.Concurrent.STM         ( TQueue
                                                , readTQueue
                                                )
import qualified Data.Aeson                    as Aeson
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
import qualified Network.Simple.TCP            as Net
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

-- TODO: Use backend config

requestsHandler
  :: LocalBackend -> TQueue SocketRequest -> (NonEmpty Task -> IO ()) -> IO ()
requestsHandler _ requests callback = forever $ do
  socketRequest <- atomically $ readTQueue requests
  case socketRequest of
    UIConfigRequest   -> error "LocalBackend is not supposed to ask for Config"
    AllTasks          -> whenNotNullM (getTasks []) callback
    ChangeTasks tasks -> saveTasks $ toList tasks

taskMonitor :: LocalBackend -> (NonEmpty Task -> IO ()) -> IO ()
taskMonitor _ newTasksCallBack = do
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
