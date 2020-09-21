module Kassandra.LocalBackend
  ( localClientSocket
  , LocalBackendRequest
  , LocalBackendResponse(DataResponse, ErrorState, SetupComplete)
  )
where

import           Control.Concurrent.STM         ( TQueue
                                                , readTQueue
                                                )

import           Kassandra.Config               ( LocalBackend )
import           Kassandra.State                ( ClientSocket )
import           Kassandra.Api                  ( SocketRequest
                                                  ( UIConfigRequest
                                                  , AllTasks
                                                  , ChangeTasks
                                                  )
                                                , SocketMessage
                                                )

data LocalSocketState = LocalError Text | SettingUp

localClientSocket
  :: TQueue LocalBackendRequest
  -> LocalBackend
  -> m (ClientSocket t m LocalSocketState)
localClientSocket requestsQueue backendConfig = undefined

--newEventWithLazyTriggerWithOnComplete
-- :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

newtype BackendError = BackendError Text
type LocalBackendRequest = Maybe (LocalBackend, LocalBackendResponse -> IO (), TQueue SocketRequest)
data LocalBackendResponse = ErrorState BackendError | SetupComplete | DataResponse SocketMessage
