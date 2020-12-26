module Backend
  ( backend
  )
where

--import           Backend.Config                 ( readConfig )
import           Control.Concurrent.STM.TChan   ( TChan
                                                , dupTChan
                                                , newBroadcastTChan
                                                , readTChan
                                                , writeTChan
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Default.Class             ( def )
import           Frontend.Route                 ( BackendRoute(..)
                                                , FrontendRoute
                                                , fullRouteEncoder
                                                )
import           Kassandra.Api                  ( SocketMessage(..)
                                                , SocketRequest(..)
                                                )
import           Kassandra.Standalone.State     ( taskMonitor )
import           Network.WebSockets             ( ServerApp
                                                , acceptRequest
                                                , forkPingThread
                                                , receiveData
                                                , rejectRequest
                                                , sendTextData
                                                )
import           Network.WebSockets.Snap        ( runWebSocketsSnap )
import           Obelisk.Backend                ( Backend(..) )
import           Say
import           Snap.Core                      ( MonadSnap )
import           Taskwarrior.IO                 ( getTasks
                                                , saveTasks
                                                )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )

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

backendSnaplet
  :: MonadSnap m => TChan (NonEmpty Task) -> R BackendRoute -> m ()
backendSnaplet broadCastChannel = \case
  BackendRouteSocket :/ (_, params) ->
    let mayUsername = (join (params ^. at "username"))
        action (Just credentials@(username, _)) | credentials `elem` users =
          acceptSocket broadCastChannel username
        action _ = \connection -> do
          putTextLn
            [i|Rejecting Websocket request #{show mayUsername :: Text}.|]
          rejectRequest connection
                        "No valid 'username' and 'password' provided."
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
  let
    sendTasks       = sendTextData connection . Aeson.encode . TaskUpdates
    sendAll         = whenNotNullM (getTasks []) sendTasks
    sendUpdates     = forever $ sendTasks =<< atomically (readTChan channel)
    waitForRequests = do
      msg <- Aeson.decode <$> receiveData connection
      concurrently_ waitForRequests $ whenJust msg $ \case
        AllTasks          -> sendAll
        ChangeTasks tasks -> saveTasks . toList $ tasks
        UIConfigRequest ->
          sayErr "Got an UI ConfigRequest, wasn‘t expecting this."
  forConcurrently_ [sendAll, sendUpdates, waitForRequests] id
