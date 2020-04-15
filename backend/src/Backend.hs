module Backend
  ( backend
  , taskMonitor
  )
where

import           Network.WebSockets             ( rejectRequest
                                                , acceptRequest
                                                , ServerApp
                                                , forkPingThread
                                                , receiveData
                                                , sendTextData
                                                )
import qualified Network.Simple.TCP            as Net
import           Network.WebSockets.Snap        ( runWebSocketsSnap )
import           Common.Api                     ( _TaskUpdates
                                                , SocketRequest
                                                  ( AllTasks
                                                  , ChangeTasks
                                                  )
                                                )
import           Common.Route                   ( BackendRoute
                                                  ( BackendRouteSocket
                                                  , BackendRouteMissing
                                                  )
                                                , FrontendRoute
                                                , fullRouteEncoder
                                                )
import           Obelisk.Backend                ( Backend(..) )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Snap.Core                      ( MonadSnap )
import qualified Data.Aeson                    as Aeson
import           Taskwarrior.IO                 ( saveTasks
                                                , getTasks
                                                )
import           Control.Concurrent.STM.TChan   ( newBroadcastTChan
                                                , dupTChan
                                                , readTChan
                                                , writeTChan
                                                , TChan
                                                )

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run          = \serve -> do
                              broadCastChannel <- atomically newBroadcastTChan
                              concurrently_
                                (taskMonitor (atomically . writeTChan broadCastChannel))
                                (serve . backendSnaplet $ broadCastChannel)
  , _backend_routeEncoder = fullRouteEncoder
  }

users :: [(Text, Text)]
users = fromList [("testUser", "hunter2")]

backendSnaplet :: MonadSnap m => TChan (NonEmpty Task) -> R BackendRoute -> m ()
backendSnaplet broadCastChannel = \case
  BackendRouteSocket :/ (_, params) ->
    let mayUsername = (join (params ^. at "username"))
        action (Just credentials@(username, _)) | credentials `elem` users =
            acceptSocket broadCastChannel username
        action _ = \connection -> do
            putTextLn [i|Rejecting Websocket request #{show mayUsername :: Text}.|]
            rejectRequest connection "No valid 'username' and 'password' provided."
    in  runWebSocketsSnap . action $ liftA2 (,)
                                            mayUsername
                                            (join (params ^. at "password"))
  BackendRouteMissing :/ () -> pass

acceptSocket :: TChan (NonEmpty Task) -> Text -> ServerApp
acceptSocket broadCastChannel user pendingConnection = do
  putStrLn [i|Websocket Client by user #{user} connected!|]
  connection <- acceptRequest pendingConnection
  forkPingThread connection 30
  channel <- atomically $ dupTChan broadCastChannel
  let sendTasks =
        sendTextData connection . Aeson.encode . (_TaskUpdates #) . toList
      sendAll         = whenNotNullM (getTasks []) sendTasks
      sendUpdates     = forever $ sendTasks =<< atomically (readTChan channel)
      waitForRequests = do
        msg <- Aeson.decode <$> receiveData connection
        concurrently_ waitForRequests $ whenJust msg $ \case
          AllTasks          -> sendAll
          ChangeTasks tasks -> saveTasks . toList $ tasks
  forConcurrently_ [sendAll, sendUpdates, waitForRequests] id

taskMonitor :: (NonEmpty Task -> IO ()) -> IO ()
taskMonitor newTasksCallBack = do
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
