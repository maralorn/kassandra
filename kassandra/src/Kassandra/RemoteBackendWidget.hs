module Kassandra.RemoteBackendWidget
  ( remoteBackendWidget
  , webClientSocket
  ) where

import           Data.Text                      ( stripPrefix )
import           Kassandra.BaseWidgets
import           Kassandra.Config               ( PasswordConfig(..)
                                                , RemoteBackend(..)
                                                )
import           Kassandra.State                ( AppContext
                                                , ClientSocket
                                                , makeStateProvider
                                                )
import           Kassandra.TextEditWidget
import           Kassandra.Types                ( WidgetJSM )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           System.Process

remoteBackendWidget
  :: forall t m
   . WidgetJSM t m
  => Maybe RemoteBackend
  -> m (R.Dynamic t (Maybe (AppContext t m)))
remoteBackendWidget mayBackend = do
  backendDyn <-
    maybe inputBackend (pure . pure) mayBackend :: m (R.Dynamic t RemoteBackend)
  responseEvent <- D.dyn
    (withBackend <$> backendDyn :: R.Dynamic t (m (Maybe (AppContext t m))))
  D.holdDyn Nothing responseEvent
 where
  inputBackend :: m (R.Dynamic t RemoteBackend)
  inputBackend = do
    protocol <- D.getLocationProtocol
    host     <- D.getLocationHost
    let defaultUrl      = protocol <> "//" <> host
        defaultUser     = "maralorn"
        defaultPassword = ""
    D.text "Host:"
    rec url <- R.holdDyn defaultUrl
          =<< enterTextWidget defaultUrl (button "selector" $ D.dynText url)
    D.text "User:"
    rec userName <- R.holdDyn defaultUser =<< enterTextWidget
          defaultUser
          (button "selector" $ D.dynText userName)
    D.text "Password:"
    rec password <- R.holdDyn defaultPassword =<< enterTextWidget
          defaultPassword
          (button "selector" $ D.dynText password)
    pure $ RemoteBackend <$> url <*> userName <*> (Password <$> password)
  withBackend :: RemoteBackend -> m (Maybe (AppContext t m))
  withBackend backend = do
    clientSocket <- webClientSocket backend
    let stateProvider = makeStateProvider clientSocket
    pure $ Just (stateProvider, D.def)
        -- TODO: Get UI Config from Server


data WebSocketState = WebSocketError Text | Connecting deriving stock Show

webClientSocket
  :: WidgetJSM t m => RemoteBackend -> m (ClientSocket t m WebSocketState)
webClientSocket backend@RemoteBackend { url, user, password } = do
  plainPassword <- case password of
    Password        plain   -> pure plain
    PasswordCommand command -> toText <$> liftIO (readCreateProcess (shell $ toString command) "")
    Prompt                  -> error "Password prompting is not implemented"
  let wsUrl = maybe "ws://localhost:8000" ("ws" <>) $ stripPrefix "http" url -- TODO: Warn user about missing http
      -- TODO: Implement alternative passwordMethods
      socketString =
        [i|#{wsUrl}/socket?username=#{user}&password=#{plainPassword}|]
  pure $ \socketRequestEvent -> do
    socket <- D.jsonWebSocket
      socketString
      ( (lensVL D.webSocketConfig_send .~ (one <$> socketRequestEvent))
      . (lensVL D.webSocketConfig_reconnect .~ False)
      $ D.def
      )
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
