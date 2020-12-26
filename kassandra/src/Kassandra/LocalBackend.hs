module Kassandra.LocalBackend
  ( localClientSocket
  , LocalBackendRequest
  , LocalBackendResponse(DataResponse, ErrorState, SetupComplete)
  ) where

import           Control.Concurrent.STM         ( TQueue
                                                , newTQueueIO
                                                , writeTQueue
                                                )

import           Kassandra.Api                  ( SocketMessage
                                                , SocketRequest(AllTasks)
                                                )
import           Kassandra.Config               ( LocalBackend )
import           Kassandra.State                ( ClientSocket )
import           Kassandra.Types                ( WidgetIO )
import qualified Reflex                        as R

data LocalSocketState = LocalError Text | SettingUp deriving Show

localClientSocket
  :: WidgetIO t m
  => TQueue LocalBackendRequest
  -> LocalBackend
  -> m (ClientSocket t m LocalSocketState)
localClientSocket requestsQueue backendConfig = do
  socketQueue <- liftIO newTQueueIO
  (backendResponseEvent, backendResponseCallback) <- R.newTriggerEvent
  let backendRequest =
        Just (backendConfig, backendResponseCallback, socketQueue)
      clientSocket socketRequestEvent = do
        let socketMessageEvent = R.fmapMaybe
              (\case
                DataResponse message -> Just message
                _                    -> Nothing
              )
              backendResponseEvent
            setupCompleteEvent = R.fmapMaybe
              (\case
                SetupComplete -> Just ()
                _             -> Nothing
              )
              backendResponseEvent
            nextStateEvent = R.fmapMaybe
              (\case
                ErrorState (BackendError err) -> Just (Left (LocalError err))
                SetupComplete                 -> Just (Right socketMessageEvent)
                _                             -> Nothing
              )
              backendResponseEvent
        R.performEvent_ $ atomically . writeTQueue socketQueue <$> R.leftmost
          [socketRequestEvent, AllTasks <$ setupCompleteEvent]
        R.holdDyn (Left SettingUp) nextStateEvent
  atomically $ writeTQueue requestsQueue backendRequest
  pure clientSocket

--newEventWithLazyTriggerWithOnComplete
-- :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

newtype BackendError = BackendError Text
type LocalBackendRequest
  = Maybe (LocalBackend, LocalBackendResponse -> IO (), TQueue SocketRequest)
data LocalBackendResponse = ErrorState BackendError | SetupComplete | DataResponse SocketMessage
