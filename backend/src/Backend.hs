{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, QuasiQuotes #-}
module Backend
  ( backend
  )
where

import           Network.WebSockets             ( acceptRequest
                                                , ServerApp
                                                , forkPingThread
                                                , receiveData
                                                , sendTextData
                                                )
import           Network.WebSockets.Snap        ( runWebSocketsSnap )
import           Common.Api                     ( SocketMessage(TaskUpdates)
                                                , SocketRequest
                                                )
import           Common.Route                   ( BackendRoute
                                                  ( BackendRoute_Socket
                                                  , BackendRoute_Missing
                                                  )
                                                , FrontendRoute
                                                , fullRouteEncoder
                                                )
import           Obelisk.Backend                ( Backend(..) )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Snap.Core                      ( MonadSnap )
import qualified Data.Aeson                    as Aeson
import           Taskwarrior.IO                 ( createTask )

backend :: Backend BackendRoute FrontendRoute
backend = Backend { _backend_run          = \serve -> serve backendSnaplet
                  , _backend_routeEncoder = fullRouteEncoder
                  }

backendSnaplet :: MonadSnap m => R BackendRoute -> m ()
backendSnaplet = \case
  BackendRoute_Socket  :/ () -> runWebSocketsSnap runSocket
  BackendRoute_Missing :/ () -> pass

runSocket :: ServerApp
runSocket = \pendingConnection -> do
  putStrLn "Connected!"
  connection <- acceptRequest pendingConnection
  forkPingThread connection 30
  let listen = do
        msg :: Maybe SocketRequest <- Aeson.decode <$> receiveData connection
        print msg
        task <- createTask "Ein Task"
        sendTextData connection $ Aeson.encode (TaskUpdates $ one task)
        listen
  listen
