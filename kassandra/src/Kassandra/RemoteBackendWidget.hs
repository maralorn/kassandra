module Kassandra.RemoteBackendWidget
  ( remoteBackendWidget
  , webClientSocket
  , CloseEvent(..)
  ) where

import           Data.Text                      ( stripPrefix )
import           JSDOM                          ( currentWindowUnchecked )
import           JSDOM.Custom.Window            ( getLocalStorage )
import           JSDOM.Generated.Storage        ( getItem
                                                , setItem
                                                )
import           Kassandra.Api                  ( SocketRequest(AllTasks) )
import           Kassandra.BaseWidgets
import           Kassandra.Config               ( PasswordConfig(..)
                                                , RemoteBackend(..)
                                                )
import           Kassandra.State                ( AppContext
                                                , ClientSocket
                                                , makeStateProvider
                                                )
import           Kassandra.Types                ( WidgetJSM )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D
import           Relude.Extra.Newtype
import           System.Process


data Connecting = LoggedOut | LoggedIn deriving (Show, Read)

newtype CloseEvent t = CloseEvent (R.Event t ()) deriving newtype (Semigroup, Monoid)

urlKey, userKey, passwordKey, loginStateKey :: String
urlKey = "Url"
userKey = "User"
passwordKey = "Password"
loginStateKey = "LoginState"

remoteBackendWidget
  :: forall t m
   . WidgetJSM t m
  => CloseEvent t
  -> Maybe (RemoteBackend PasswordConfig)
  -> m (R.Dynamic t (Maybe (AppContext t m)))
remoteBackendWidget closeEvent mayBackend = do
  backendDyn    <- maybe inputBackend getPassword mayBackend
  D.dynText (show . isNothing <$> backendDyn)
  responseEvent <- D.dyn
    (   withBackend (closeEvent <> wrap (() <$ R.updated backendDyn))
    <$> backendDyn
    )
  D.holdDyn Nothing responseEvent
 where
  getPassword
    :: RemoteBackend PasswordConfig
    -> m (R.Dynamic t (Maybe (RemoteBackend Text)))
  getPassword RemoteBackend { url, user, password } = do
    fmap (pure . RemoteBackend url user) <$> case password of
      Password        plain   -> pure (pure plain)
      PasswordCommand command -> pure . Text <$> liftIO
        (readCreateProcess (shell $ toString command) "")
      Prompt -> do
        D.text [i|Enter password for #{user} on #{url}:|]
        undefined
  inputBackend :: m (R.Dynamic t (Maybe (RemoteBackend Text)))
  inputBackend = do
    protocol <- D.getLocationProtocol
    host     <- D.getLocationHost
    let defaultUrl      = protocol <> "//" <> host
        defaultUser     = "maralorn"
        defaultPassword = ""
    storage      <- getStorage
    initialState <-
      fromMaybe LoggedOut . (readMaybe =<<) <$> getItem storage loginStateKey
    backendEvent <- stateWidget initialState $ \loginState -> do
      setItem storage ("LoginState" :: String) (show loginState :: String)
      now           <- R.getPostBuild
      maybeUrl      <- getItem storage urlKey
      maybeUser     <- getItem storage userKey
      maybePassword <- getItem storage passwordKey
      case loginState of
        LoggedOut -> do
          D.text "Host:"
          urlInput <- textInput False (fromMaybe defaultUrl maybeUrl)
          D.el "br" pass
          D.text "User:"
          userNameInput <- textInput False (fromMaybe defaultUser maybeUser)
          D.el "br" pass
          D.text "Password:"
          passwordInput <- textInput True
                                     (fromMaybe defaultPassword maybePassword)
          D.el "br" pass
          saveButton <- button "selector" $ D.text "Login"
          let
            saveEvent = fold
              ( saveButton
              : (   D.keypress D.Enter
                <$> [urlInput, userNameInput, passwordInput]
                )
              )
          R.performEvent_
            $   (    liftA3 (,,)
                            (inputValue urlInput)
                            (inputValue userNameInput)
                            (inputValue passwordInput)
                R.<@ saveEvent
                )
            <&> \(url, userName, password) -> do
                  setItem storage urlKey      url
                  setItem storage userKey     userName
                  setItem storage passwordKey password
          pure (Nothing <$ now, LoggedIn <$ saveEvent)
        LoggedIn -> do
          (backendEvent, errorEvent) <-
            case (maybeUrl, maybeUser, maybePassword) of
              (Just url, Just user, Just password) ->
                D.text [i|#{user} @ #{url}|]
                  $> (Just (RemoteBackend url user password) <$ now, R.never)
              _ -> pure (R.never, now)
          manualLogoutEvent <- button "selector" $ D.text "Logout"
          let logoutEvent = manualLogoutEvent <> errorEvent
          pure (backendEvent, LoggedOut <$ logoutEvent)
    R.holdDyn Nothing backendEvent
  withBackend
    :: CloseEvent t -> Maybe (RemoteBackend Text) -> m (Maybe (AppContext t m))
  withBackend innerCloseEvent (Just backend) = do
    clientSocket <- webClientSocket innerCloseEvent backend
    let stateProvider = makeStateProvider clientSocket
    pure $ Just (stateProvider, D.def)
  withBackend _ Nothing = pure Nothing
  getStorage = getLocalStorage =<< currentWindowUnchecked
        -- TODO: Get UI Config from Server
  textInput hidden defaultValue =
    D.inputElement
      $  D.def
      &  lensVL D.inputElementConfig_initialValue
      .~ defaultValue
      &  lensVL
           ( D.inputElementConfig_elementConfig
           . D.elementConfig_initialAttributes
           )
      .~ if hidden then "type" D.=: "password" else mempty
  inputValue = R.current . D._inputElement_value

data WebSocketState = WebSocketError Text | Connecting deriving stock Show

webClientSocket
  :: WidgetJSM t m => CloseEvent t -> RemoteBackend Text -> m (ClientSocket t m)
webClientSocket closeEvent backend@RemoteBackend { url, user, password } = do
  let wsUrl = maybe "ws://localhost:8000" ("ws" <>) $ stripPrefix "http" url -- TODO: Warn user about missing http
      socketString = [i|#{wsUrl}/socket?username=#{user}&password=#{password}|]
  refreshEvent <- button "Refresh Tasks" $ D.text "Refresh Tasks"
  pure $ \socketRequestEvent -> do
    socket <- D.jsonWebSocket
      socketString
      ( (  lensVL D.webSocketConfig_send
        .~ foldMap (fmap one) [socketRequestEvent, AllTasks <$ refreshEvent]
        )
      . (lensVL D.webSocketConfig_reconnect .~ True)
      . (  lensVL D.webSocketConfig_close
        .~ ((0, "Client unloaded websocket.") <$ un closeEvent)
        )
      $ D.def
      )
    let
      close =
        Just
            [i|Connection to kassandra server not possible. Check network connection, server url and credentials. #{backend}|]
          <$ socket
          ^. lensVL D.webSocket_close
      open = Nothing <$ socket ^. lensVL D.webSocket_open
      messageParseFail =
        maybe (Just "Failed to parse SocketMessage JSON") (const Nothing)
          <$> socket
          ^.  lensVL D.webSocket_recv
      nextStateEvent = R.leftmost [messageParseFail, close, open]
    D.dynText
      .   fmap (fromMaybe "")
      =<< R.holdDyn (Just "Websocket Connecting") nextStateEvent
    pure . pure . R.fmapMaybe id $ socket ^. lensVL D.webSocket_recv
