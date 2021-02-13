module Kassandra.LocalBackend (
  localClientSocket,
  LocalBackendRequest (LocalBackendRequest, userConfig, alive, responseCallback, requestQueue),
  LocalBackendResponse (DataResponse, ErrorState, SetupComplete),
  BackendError (..),
) where

import Control.Concurrent.STM (TQueue, newTQueueIO, writeTQueue)
import Kassandra.Api (SocketMessage, SocketRequest (AllTasks))
import Kassandra.Config (UserConfig)
import Kassandra.State (ClientSocket)
import Kassandra.Types (WidgetIO)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Newtype

data LocalSocketState = LocalError Text | SettingUp deriving (Show)
makePrismLabels ''LocalSocketState
newtype BackendError = BackendError Text

data LocalBackendResponse = ErrorState BackendError | SetupComplete | DataResponse SocketMessage
makePrismLabels ''LocalBackendResponse

data LocalBackendRequest = LocalBackendRequest
  { userConfig :: UserConfig
  , alive :: TVar Bool
  , responseCallback :: LocalBackendResponse -> IO ()
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
  (backendResponseEvent, responseCallback) <- R.newTriggerEvent
  alive <- newTVarIO True
  let backendRequest =
        LocalBackendRequest
          { userConfig
          , alive
          , responseCallback
          , requestQueue
          }
      socketMessageEvent = (^? #_DataResponse) <$?> backendResponseEvent
      setupCompleteEvent = (^? #_SetupComplete) <$?> backendResponseEvent
      errorEvent = (^? #_ErrorState) <$> backendResponseEvent
      clientSocket socketRequestEvent = do
        D.dynText . fmap (fromMaybe "" . un) =<< R.holdDyn Nothing errorEvent
        R.performEvent_ $ atomically . writeTQueue requestQueue <$> R.leftmost [socketRequestEvent, AllTasks <$ setupCompleteEvent]
        pure (pure socketMessageEvent)
  atomically $ writeTQueue requestsQueue backendRequest
  pure clientSocket
