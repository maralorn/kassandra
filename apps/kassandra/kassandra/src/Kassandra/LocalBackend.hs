module Kassandra.LocalBackend (
  localClientSocket,
  LocalBackendRequest (LocalBackendRequest, userConfig, alive, responseCallback, requestQueue),
  BackendError (..),
) where

import Control.Concurrent.STM (TQueue, newTQueueIO, writeTQueue)
import Kassandra.Api (SocketMessage, SocketRequest)
import Kassandra.Config (UserConfig)
import Kassandra.State (ClientSocket)
import Kassandra.Types (WidgetIO)
import qualified Reflex as R

data LocalSocketState = LocalError Text | SettingUp deriving (Show)
makePrismLabels ''LocalSocketState

newtype BackendError = BackendError Text

data LocalBackendRequest = LocalBackendRequest
  { userConfig :: UserConfig
  , alive :: TVar Bool
  , responseCallback :: SocketMessage -> IO ()
  , requestQueue :: TQueue SocketRequest
  }
makeLabels ''LocalBackendRequest

localClientSocket ::
  WidgetIO t m =>
  TQueue LocalBackendRequest ->
  UserConfig ->
  m (ClientSocket t m)
localClientSocket requestsQueue userConfig = do
  requestQueue <- liftIO newTQueueIO
  (socketMessageEvent, responseCallback) <- R.newTriggerEvent
  alive <- newTVarIO True
  let backendRequest = LocalBackendRequest{userConfig, alive, responseCallback, requestQueue}
      clientSocket socketRequestEvent = do
        R.performEvent_ $ atomically . mapM_ (writeTQueue requestQueue) <$> socketRequestEvent
        pure (pure socketMessageEvent)
  atomically $ writeTQueue requestsQueue backendRequest
  pure clientSocket
