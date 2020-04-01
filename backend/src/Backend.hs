{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}
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
import           Common.Api                     ( _TaskUpdates
                                                , SocketRequest
                                                  ( AllTasks
                                                  , ChangeTasks
                                                  )
                                                )
import           Common.Route                   ( BackendRoute
                                                  ( BackendRouteSocket
                                                  , BackendRouteMissing
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
import           Taskwarrior.IO                 ( saveTasks
                                                , getTasks
                                                )

backend :: Backend BackendRoute FrontendRoute
backend = Backend { _backend_run          = \serve -> serve backendSnaplet
                  , _backend_routeEncoder = fullRouteEncoder
                  }

backendSnaplet :: MonadSnap m => R BackendRoute -> m ()
backendSnaplet = \case
  BackendRouteSocket  :/ () -> runWebSocketsSnap runSocket
  BackendRouteMissing :/ () -> pass

runSocket :: ServerApp
runSocket pendingConnection = do
  putStrLn "Websocket Client connected!"
  connection <- acceptRequest pendingConnection
  forkPingThread connection 30
  void $ forkIO $ do
    tasks <- getTasks []
    sendTextData connection . Aeson.encode . (_TaskUpdates #) $ tasks
  let listen = do
        msg <- Aeson.decode <$> receiveData connection
        whenJust msg $ \case
          AllTasks ->
            putTextLn "Client wants all Tasks! (backend unimplemented)"
          ChangeTasks tasks -> saveTasks . toList $ tasks
        listen
  listen
