module Frontend
  ( frontend
  ) where


import           Obelisk.Frontend
import           Obelisk.Route

import qualified Data.HashMap.Strict           as HashMap
import           Frontend.Route                 ( FrontendRoute )
import           Kassandra.Api                  ( _ChangeTasks
                                                , _TaskUpdates
                                                )
import           Kassandra.Config               ( NamedBackend(..)
                                                , PasswordConfig(..)
                                                , RemoteBackend(..)
                                                , TreeOption(..)
                                                , UIConfig(..)
                                                , UIFeatures(..)
                                                )
import           Kassandra.Css                  ( cssAsText )
import           Kassandra.MainWidget           ( mainWidget )
import           Kassandra.RemoteBackendWidget  ( remoteBackendWidget )
import           Kassandra.Types
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend { _frontend_head = frontendHead
                    , _frontend_body = void $ D.prerender pass frontendBody
                    }

frontendBody :: WidgetJSM t m => m ()
frontendBody = do
  protocol <- D.getLocationProtocol
  host     <- D.getLocationHost
  D.dyn_
    . (maybe pass
             (\(stateProvider, uiConfig) -> mainWidget uiConfig stateProvider) <$>
      )
    =<< remoteBackendWidget
          (NamedBackend
            "DirectRemote"
            (RemoteBackend (protocol <> "//" <> host) "testUser" (Password "hunter2"))
          )


   {-
type WidgetJSM t m
  = (D.HasJSContext m, MonadJSM (R.Performable m), MonadJSM m, WidgetIO t m)

webSocketStateProvider :: WidgetJSM t m => StateProvider t m
webSocketStateProvider = stateProvider webSocketTaskProvider

webSocketTaskProvider :: forall t m . WidgetJSM t m => TaskProvider t m
webSocketTaskProvider changeTasksEvent = do
  protocol <- D.getLocationProtocol
  host     <- D.getLocationHost
  socket   <- D.jsonWebSocket
    (if protocol == "http:"
      then [i|ws://#{host}/socket?username=testUser&password=hunter2|]
      else [i|wss://#{host}/socket?username=testUser&password=hunter2|]
    )
    (  lensVL D.webSocketConfig_send
    .~ (one . (_ChangeTasks #) <$> changeTasksEvent)
    $  D.def
    )
  let
    close =
      "Connection to kassandra server not possible. Check network connection, server url and credentials."
        <$ socket
        ^. lensVL D.webSocket_close
    open =
      "Connected to kassandra server!" <$ socket ^. lensVL D.webSocket_open
  websocketState <- R.holdDyn "Connecting to kassandra server..."
                              (R.leftmost [close, open])
  D.dynText websocketState
  let updateTasksEvents =
        R.fmapMaybe ((nonEmpty =<<) . (^? _TaskUpdates) =<<)
          $  socket
          ^. lensVL D.webSocket_recv
  R.foldDyn (flip . foldr . join $ HashMap.insert . (^. #uuid))
            HashMap.empty
            (changeTasksEvent <> updateTasksEvents)
-}

frontendHead :: ObeliskWidget js t route m => m ()
frontendHead = do
  D.el "title" $ D.text "Kassandra 2 Webversion"
  D.elAttr "style" mempty $ D.text cssAsText
