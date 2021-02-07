module Kassandra.LocalBackend (
  localClientSocket,
  LocalBackendRequest,
  LocalBackendResponse (DataResponse, ErrorState, SetupComplete),
) where

import Control.Concurrent.STM (
  TQueue,
  newTQueueIO,
  writeTQueue,
 )
import Kassandra.Api (SocketRequest (AllTasks))
import Kassandra.Config (LocalBackend)
import Kassandra.State (ClientSocket)
import Kassandra.Types (WidgetIO)
import qualified Reflex as R
import qualified Reflex.Dom as D

data LocalSocketState = LocalError Text | SettingUp deriving (Show)

localClientSocket ::
  WidgetIO t m =>
  TQueue LocalBackendRequest ->
  LocalBackend ->
  m (ClientSocket t m)
localClientSocket requestsQueue backendConfig = do
  socketQueue <- liftIO newTQueueIO
  (backendResponseEvent, backendResponseCallback) <- R.newTriggerEvent
  let backendRequest =
        Just (backendConfig, backendResponseCallback, socketQueue)
      clientSocket socketRequestEvent = do
        let socketMessageEvent =
              R.fmapMaybe
                ( \case
                    DataResponse message -> Just message
                    _ -> Nothing
                )
                backendResponseEvent
            setupCompleteEvent =
              R.fmapMaybe
                ( \case
                    SetupComplete -> Just ()
                    _ -> Nothing
                )
                backendResponseEvent
            errorEvent =
              backendResponseEvent <&> \case
                ErrorState (BackendError err) -> Just err
                _ -> Nothing
        D.dynText . fmap (fromMaybe "") =<< R.holdDyn Nothing errorEvent
        R.performEvent_ $
          atomically . writeTQueue socketQueue
            <$> R.leftmost
              [socketRequestEvent, AllTasks <$ setupCompleteEvent]
        pure (pure socketMessageEvent)
  atomically $ writeTQueue requestsQueue backendRequest
  pure clientSocket

newtype BackendError = BackendError Text
type LocalBackendRequest =
  Maybe (LocalBackend, LocalBackendResponse -> IO (), TQueue SocketRequest)
data LocalBackendResponse = ErrorState BackendError | SetupComplete | DataResponse (NonEmpty Task)
