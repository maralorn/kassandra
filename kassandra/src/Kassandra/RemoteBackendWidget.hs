module Kassandra.RemoteBackendWidget
  ( remoteBackendWidget
  , webClientSocket
  ) where

import           Data.Text                      ( stripPrefix )
import           Kassandra.Config               ( NamedBackend(..)
                                                , PasswordConfig(..)
                                                , RemoteBackend(..)
                                                )
import           Kassandra.State                ( AppContext
                                                , ClientSocket
                                                , makeStateProvider
                                                )
import           Kassandra.Types                ( WidgetJSM )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Relude.Unsafe                  ( fromJust )

remoteBackendWidget
  :: WidgetJSM t m
  => NamedBackend RemoteBackend
  -> m (R.Dynamic t (Maybe (AppContext t m)))
remoteBackendWidget NamedBackend { backend } = do
  clientSocket <- webClientSocket backend
  let stateProvider = makeStateProvider clientSocket
  pure $ pure $ Just (stateProvider, D.def)
  -- TODO: Get UI Config from Server

data WebSocketState = WebSocketError Text | Connecting deriving stock Show

webClientSocket
  :: WidgetJSM t m => RemoteBackend -> m (ClientSocket t m WebSocketState)
webClientSocket backend@RemoteBackend { url, user, password } = do
  let
    wsUrl         = "ws" <> fromJust (stripPrefix "http" url)
    plainPassword = case password of
      Password plain -> plain
      _ -> "PasswordMissing"
    -- TODO: Implement alternative passwordMethods
    socketString =
      [i|#{wsUrl}/socket?username=#{user}&password=#{plainPassword}|]
  pure $ \socketRequestEvent -> do
    socket <- D.jsonWebSocket
      socketString
      (lensVL D.webSocketConfig_send .~ (one <$> socketRequestEvent) $ D.def)
    let
      close =
        Left
            (WebSocketError
              [i|Connection to kassandra server not possible. Check network connection, server url and credentials. #{backend}|]
            )
          <$ socket
          ^. lensVL D.webSocket_close
      open          = Right socketMessage <$ socket ^. lensVL D.webSocket_open
      socketMessage = R.fmapMaybe id $ socket ^. lensVL D.webSocket_recv
      messageParseFail =
        R.fmapMaybe
            (maybe
              (Just (Left (WebSocketError "Failed to parse SocketMessage JSON"))
              )
              (const Nothing)
            )
          $  socket
          ^. lensVL D.webSocket_recv
      nextStateEvent = R.leftmost [messageParseFail, close, open]
    R.holdDyn (Left Connecting) nextStateEvent
