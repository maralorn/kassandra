module Kassandra.RemoteBackendWidget
  ( remoteBackendWidget
  , webClientSocket
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.JSString                  ( unpack )
import           Data.Text                      ( stripPrefix )
import           JSDOM
import           JSDOM.Custom.Window            ( getLocalStorage )
import           JSDOM.Generated.Storage        ( getItem
                                                , setItem
                                                )
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
  => Maybe (RemoteBackend PasswordConfig)
  -> m (R.Dynamic t (Maybe (AppContext t m)))
remoteBackendWidget mayBackend = do
  backendDyn    <- maybe inputBackend getPassword mayBackend
  responseEvent <- D.dyn (withBackend <$> backendDyn)
  D.holdDyn Nothing responseEvent
 where
  getPassword
    :: RemoteBackend PasswordConfig -> m (R.Dynamic t (RemoteBackend Text))
  getPassword RemoteBackend { url, user, password } = do
    fmap (RemoteBackend url user) <$> case password of
      Password        plain   -> pure (pure plain)
      PasswordCommand command -> pure . Text <$> liftIO
        (readCreateProcess (shell $ toString command) "")
      Prompt -> do
        D.text [i|Enter password for #{user} on #{url}:|]
        prompt "password" ""

  inputBackend :: m (R.Dynamic t (RemoteBackend Text))
  inputBackend = do
    protocol <- D.getLocationProtocol
    host     <- D.getLocationHost
    let defaultUrl      = protocol <> "//" <> host
        defaultUser     = "maralorn"
        defaultPassword = ""
    D.text "Host:"
    url <- prompt "host" defaultUrl
    D.text "User:"
    userName <- prompt "user" defaultUser
    D.text "Password:"
    password <- prompt "password" defaultPassword
    pure $ RemoteBackend <$> url <*> userName <*> password
  withBackend :: RemoteBackend Text -> m (Maybe (AppContext t m))
  withBackend backend = do
    clientSocket <- webClientSocket backend
    let stateProvider = makeStateProvider clientSocket
    pure $ Just (stateProvider, D.def)
        -- TODO: Get UI Config from Server

prompt :: WidgetJSM t m => Text -> Text -> m (R.Dynamic t Text)
prompt label defaultVal = do
  rec val <-
        fromMaybe defaultVal
          <<$>> (localStorage label =<< enterTextWidget
                  defaultVal
                  (button "selector" $ D.dynText val)
                )
  pure val

localStorage
  :: (ToJSON a, FromJSON a, WidgetJSM t m)
  => Text
  -> R.Event t a
  -> m (R.Dynamic t (Maybe a))
localStorage label setEvent = do
  storage <- getLocalStorage =<< currentWindowUnchecked
  R.performEvent_ $ setEvent <&> \value -> do
    setItem storage label (decodeUtf8 @Text $ encode value)
  item <- (decode . encodeUtf8 @Text =<<) <$> getItem storage label
  R.holdDyn item (pure <$> setEvent)

data WebSocketState = WebSocketError Text | Connecting deriving stock Show

webClientSocket
  :: WidgetJSM t m => RemoteBackend Text -> m (ClientSocket t m WebSocketState)
webClientSocket backend@RemoteBackend { url, user, password } = do
  let wsUrl = maybe "ws://localhost:8000" ("ws" <>) $ stripPrefix "http" url -- TODO: Warn user about missing http
      socketString = [i|#{wsUrl}/socket?username=#{user}&password=#{password}|]
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
