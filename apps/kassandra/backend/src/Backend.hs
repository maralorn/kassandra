module Backend (
  backend,
) where

import Backend.Config (BackendConfig, readConfig, users)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue)
import Control.Exception (try)
import qualified Data.Aeson as Aeson
import Data.Map (lookup)
import Data.Password.Argon2
import Frontend.Route (BackendRoute (..), FrontendRoute, fullRouteEncoder)
import Kassandra.Config (AccountConfig (..))
import Kassandra.LocalBackend (LocalBackendRequest (..))
import Kassandra.Standalone.State (localBackendProvider)
import Network.WebSockets (ConnectionException, ServerApp, acceptRequest, forkPingThread, receiveData, rejectRequest, sendTextData)
import Network.WebSockets.Snap (runWebSocketsSnap)
import Obelisk.Backend (Backend (..))
import Obelisk.Route (R, pattern (:/))
import Say (say)
import Snap.Core (MonadSnap, Snap)

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = serveSnaplet
    , _backend_routeEncoder = fullRouteEncoder
    }

serveSnaplet :: ((R BackendRoute -> Snap ()) -> IO b) -> IO ()
serveSnaplet serve = do
  config <- readConfig Nothing
  backendRequestQueue <- newTQueueIO
  let backendSnaplet :: MonadSnap m => R BackendRoute -> m ()
      backendSnaplet = \case
        BackendRouteSocket :/ (_, params) -> serveWebsocket config backendRequestQueue params
        BackendRouteMissing :/ () -> pass
  concurrently_
    (localBackendProvider backendRequestQueue)
    (serve backendSnaplet)

serveWebsocket ::
  MonadSnap m =>
  BackendConfig ->
  TQueue LocalBackendRequest ->
  Map Text (Maybe Text) ->
  m ()
serveWebsocket config backendRequestQueue params =
  let mayUsername = join (params ^. at "username")
      mayPassword = join (params ^. at "password")
      mayCreds = liftA2 (,) mayUsername mayPassword
      action (Just (username, password))
        | Just userConfig <- lookup username (users config)
          , PasswordCheckSuccess <- checkPassword (mkPassword password) (passwordHash userConfig) =
          acceptSocket backendRequestQueue username userConfig
      action (Just (username, _))
        | Just _ <- lookup username (users config) =
          \connection -> do
            say [i|Rejecting Websocket request for #{username :: Text}, wrong password.|]
            rejectRequest connection "No valid 'username' and 'password' provided."
      action _ = \connection -> do
        say [i|Rejecting Websocket request #{show mayUsername :: Text}. No matching user found.|]
        rejectRequest connection "No valid 'username' and 'password' provided."
   in runWebSocketsSnap . action $ mayCreds

acceptSocket :: TQueue LocalBackendRequest -> Text -> AccountConfig -> ServerApp
acceptSocket backendRequestQueue username accountConfig pendingConnection = do
  say [i|Websocket Client by user #{username} connected!|]
  connection <- acceptRequest pendingConnection
  forkPingThread connection 30
  let responseCallback = sendTextData connection . Aeson.encode
  alive <- newTVarIO True
  requestQueue <- newTQueueIO
  atomically . writeTQueue backendRequestQueue $
    LocalBackendRequest
      { userConfig = accountConfig ^. #userConfig
      , alive
      , responseCallback
      , requestQueue
      }
  let go =
        (try (receiveData connection) :: IO (Either ConnectionException LByteString)) >>= \case
          Left err -> do
            say [i|Socket for #{username} closed. With error #{err}|]
            atomically (writeTVar alive False)
          Right (Aeson.decode -> msg) ->
            concurrently_ go . whenJust msg $ atomically . writeTQueue requestQueue
  go
