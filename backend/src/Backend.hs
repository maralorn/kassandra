module Backend (
  backend,
) where

import Backend.Config
import Control.Concurrent.STM.TChan (
  TChan,
  dupTChan,
  newBroadcastTChan,
  readTChan,
  writeTChan,
 )
import qualified Data.Aeson as Aeson
import Data.Default.Class (def)
import Data.Map (lookup)
import Data.Password.Argon2
import Frontend.Route (
  BackendRoute (..),
  FrontendRoute,
  fullRouteEncoder,
 )
import Kassandra.Api (
  SocketMessage (..),
  SocketRequest (..),
 )
import Kassandra.Config hiding (
  backend,
  id,
 )
import Kassandra.Standalone.State (taskMonitor)
import Network.WebSockets (
  ServerApp,
  acceptRequest,
  forkPingThread,
  receiveData,
  rejectRequest,
  sendTextData,
 )
import Network.WebSockets.Snap (runWebSocketsSnap)
import Obelisk.Backend (Backend (..))
import Obelisk.Route (
  R,
  pattern (:/),
 )
import Say
import Snap.Core (MonadSnap)
import Taskwarrior.IO (
  getTasks,
  saveTasks,
 )

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do
        config <- readConfig Nothing
        broadCastChannel <- atomically newBroadcastTChan
        concurrently_
          (taskMonitor def (atomically . writeTChan broadCastChannel))
          (serve . backendSnaplet config $ broadCastChannel)
    , _backend_routeEncoder = fullRouteEncoder
    }

backendSnaplet ::
  MonadSnap m =>
  BackendConfig ->
  TChan (NonEmpty Task) ->
  R BackendRoute ->
  m ()
backendSnaplet config broadCastChannel = \case
  BackendRouteSocket :/ (_, params) ->
    let mayUsername = join (params ^. at "username")
        mayPassword = join (params ^. at "password")
        mayCreds = liftA2 (,) mayUsername mayPassword
        action (Just (username, password))
          | Just userConfig <- lookup username (users config)
            , PasswordCheckSuccess <-
                checkPassword
                  (mkPassword password)
                  (passwordHash userConfig) =
            acceptSocket broadCastChannel username userConfig
        action (Just (username, _))
          | Just _ <- lookup username (users config) =
            \connection -> do
              putTextLn
                [i|Rejecting Websocket request for #{username :: Text}, wrong password.|]
              rejectRequest
                connection
                "No valid 'username' and 'password' provided."
        action _ = \connection -> do
          putTextLn
            [i|Rejecting Websocket request #{show mayUsername :: Text}. No matching user found.|]
          rejectRequest
            connection
            "No valid 'username' and 'password' provided."
     in runWebSocketsSnap . action $ mayCreds
  BackendRouteMissing :/ () -> pass

acceptSocket :: TChan (NonEmpty Task) -> Text -> AccountConfig -> ServerApp
acceptSocket broadCastChannel username userConfig pendingConnection = do
  putStrLn [i|Websocket Client by user #{username} connected!|]
  connection <- acceptRequest pendingConnection
  forkPingThread connection 30
  channel <- atomically $ dupTChan broadCastChannel
  let sendTasks = sendTextData connection . Aeson.encode . TaskUpdates
      sendAll = whenNotNullM (getTasks []) sendTasks
      sendUpdates = forever $ sendTasks =<< atomically (readTChan channel)
      waitForRequests = do
        msg <- Aeson.decode <$> receiveData connection
        concurrently_ waitForRequests $
          whenJust msg $ \case
            AllTasks -> sendAll
            ChangeTasks tasks -> saveTasks . toList $ tasks
            UIConfigRequest ->
              sayErr "Got an UI ConfigRequest, wasn‘t expecting this."
  forConcurrently_ [sendAll, sendUpdates, waitForRequests] id
