module Backend
  ( backend
  )
where

import           Network.WebSockets             ( rejectRequest
                                                , acceptRequest
                                                , ServerApp
                                                , forkPingThread
                                                , receiveData
                                                , sendTextData
                                                )
import           Network.WebSockets.Snap        ( runWebSocketsSnap )
import           Kassandra.Api                     ( SocketMessage(TaskUpdates)
                                                , SocketRequest
                                                  ( AllTasks
                                                  , ChangeTasks
                                                  )
                                                )
import           Frontend.Route                   ( BackendRoute
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
import           Backend.Config                 ( readConfig )
import Data.Default.Class (def)
import Kassandra.Standalone.State (taskMonitor)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run          = \serve -> do
                              --config <- readConfig Nothing
                              broadCastChannel <- atomically newBroadcastTChan
                              concurrently_
                                (taskMonitor def (atomically . writeTChan broadCastChannel))
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
        sendTextData connection . Aeson.encode . TaskUpdates
      sendAll         = whenNotNullM (getTasks []) sendTasks
      sendUpdates     = forever $ sendTasks =<< atomically (readTChan channel)
      waitForRequests = do
        msg <- Aeson.decode <$> receiveData connection
        concurrently_ waitForRequests $ whenJust msg $ \case
          AllTasks          -> sendAll
          ChangeTasks tasks -> saveTasks . toList $ tasks
  forConcurrently_ [sendAll, sendUpdates, waitForRequests] id
