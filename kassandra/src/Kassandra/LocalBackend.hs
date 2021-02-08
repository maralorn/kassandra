module Kassandra.LocalBackend (
  localClientSocket,
  LocalBackendRequest,
  LocalBackendResponse (DataResponse, ErrorState, SetupComplete),
) where

import Control.Concurrent.STM (TQueue, newTQueueIO, writeTQueue)
import Kassandra.Api (SocketRequest (AllTasks))
import Kassandra.Config (LocalBackend)
import Kassandra.State (ClientSocket)
import Kassandra.Types (WidgetIO)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Newtype

data LocalSocketState = LocalError Text | SettingUp deriving (Show)
makePrismLabels ''LocalSocketState
newtype BackendError = BackendError Text
type LocalBackendRequest = Maybe (LocalBackend, LocalBackendResponse -> IO (), TQueue SocketRequest)
data LocalBackendResponse = ErrorState BackendError | SetupComplete | DataResponse (NonEmpty Task)
makePrismLabels ''LocalBackendResponse

localClientSocket ::
  WidgetIO t m =>
  TQueue LocalBackendRequest ->
  LocalBackend ->
  m (ClientSocket t m)
localClientSocket requestsQueue backendConfig = do
  socketQueue <- liftIO newTQueueIO
  (backendResponseEvent, backendResponseCallback) <- R.newTriggerEvent
  let backendRequest = Just (backendConfig, backendResponseCallback, socketQueue)
      socketMessageEvent = (#_Tasks #) <<$>> (^? #_DataResponse) <$?> backendResponseEvent
      setupCompleteEvent = (^? #_SetupComplete) <$?> backendResponseEvent
      errorEvent = (^? #_ErrorState) <$> backendResponseEvent
      clientSocket socketRequestEvent = do
        D.dynText . fmap (fromMaybe "" . un) =<< R.holdDyn Nothing errorEvent
        R.performEvent_ $ atomically . writeTQueue socketQueue <$> R.leftmost [socketRequestEvent, AllTasks <$ setupCompleteEvent]
        pure (pure socketMessageEvent)
  atomically $ writeTQueue requestsQueue backendRequest
  pure clientSocket
