module Kassandra.RemoteBackendWidget
  ( remoteBackendWidget
  , webClientSocket
  )
where

import qualified Reflex                        as R
import           Kassandra.Config               ( RemoteBackend, NamedBackend )
import           Kassandra.State                ( AppContext
                                                , ClientSocket
                                                )
import           Kassandra.Types                ( Widget )

remoteBackendWidget
  :: Widget t m
  => NamedBackend RemoteBackend
  -> m (R.Dynamic t (Maybe (AppContext t m)))
remoteBackendWidget = undefined

data WebSocketState = WebSocketError Text | Connecting

webClientSocket :: RemoteBackend -> m (ClientSocket t m WebSocketState)
webClientSocket = undefined
