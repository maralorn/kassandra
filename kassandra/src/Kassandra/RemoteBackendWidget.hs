module Kassandra.RemoteBackendWidget (
  remoteBackendWidget,
  webClientSocket,
  CloseEvent (..),
) where

import Data.Aeson (decode, encode)
import Data.Map (elems, insert)
import Data.Text (stripPrefix)
import JSDOM (currentWindowUnchecked)
import JSDOM.Custom.Window (getLocalStorage)
import JSDOM.Generated.Storage (Storage, getItem, setItem)
import Kassandra.Api (
  SocketMessage,
  SocketRequest (AllTasks),
 )
import Kassandra.BaseWidgets (button, stateWidget)
import Kassandra.Config (
  PasswordConfig (..),
  RemoteBackend (..),
 )
import Kassandra.State (
  ClientSocket,
  StateProvider,
  makeStateProvider,
 )
import Kassandra.Types (WidgetJSM)
import Language.Javascript.JSaddle (liftJSM)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Newtype (un, wrap)
import System.Process (readCreateProcess, shell)

data Connecting = LoggedOut | LoggedIn deriving (Show, Read)

newtype CloseEvent t = CloseEvent (R.Event t ()) deriving newtype (Semigroup, Monoid)

urlKey, userKey, passwordKey, loginStateKey :: String
urlKey = "Url"
userKey = "User"
passwordKey = "Password"
loginStateKey = "LoginState"

remoteBackendWidget ::
  forall t m.
  WidgetJSM t m =>
  CloseEvent t ->
  Maybe (RemoteBackend PasswordConfig) ->
  m (R.Dynamic t (Maybe (StateProvider t m)))
remoteBackendWidget closeEvent mayBackend = D.divClass "remoteBackend" $ do
  backendDyn <- maybe inputBackend getPassword mayBackend
  responseEvent <-
    D.dyn
      (withBackend (closeEvent <> wrap (() <$ R.updated backendDyn)) <$> backendDyn)
  D.holdDyn Nothing responseEvent
 where
  getPassword :: RemoteBackend PasswordConfig -> m (R.Dynamic t (Maybe (RemoteBackend Text)))
  getPassword RemoteBackend{url, user, password} = do
    fmap (pure . RemoteBackend url user) <$> case password of
      Password plain -> pure (pure plain)
      PasswordCommand command -> pure . Text <$> liftIO (readCreateProcess (shell $ toString command) "")
      Prompt -> do
        D.text [i|Enter password for #{user} on #{url}:|]
        storage <- getStorage
        initialPassword <- fromMaybe "" <$> getItem storage passwordKey
        passwordInput <- textInput True initialPassword
        let sendEvent = R.tag (inputValue passwordInput) (D.keypress D.Enter passwordInput)
        R.performEvent_ $ sendEvent <&> setItem storage ([i|PasswordFor#{user}On#{url}|] :: String) . toString
        R.holdDyn "" sendEvent
  inputBackend :: m (R.Dynamic t (Maybe (RemoteBackend Text)))
  inputBackend = do
    protocol <- D.getLocationProtocol
    host <- D.getLocationHost
    let defaultUrl = protocol <> "//" <> host
        defaultUser = ""
        defaultPassword = ""
    storage <- getStorage
    initialUrl <- fromMaybe defaultUrl <$> getItem storage urlKey
    initialUser <- fromMaybe defaultUser <$> getItem storage userKey
    initialPassword <- fromMaybe defaultPassword <$> getItem storage passwordKey
    initialState <- fromMaybe LoggedOut . (readMaybe =<<) <$> getItem storage loginStateKey
    stateEvent <- stateWidget (initialState, initialUrl, initialUser, initialPassword) stateTransition
    R.performEvent_ $
      stateEvent <&> \(loginState, url, user, password) -> do
        setItem storage loginStateKey (show loginState :: String)
        setItem storage urlKey url
        setItem storage userKey user
        setItem storage passwordKey password
    fmap backendFromState <$> R.holdDyn (initialState, initialUrl, initialUser, initialPassword) stateEvent
  stateTransition (loginState, url, user, password) = case loginState of
    LoggedOut -> D.divClass "loginDialog" $ do
      D.text "Host:"
      urlInput <- textInput False url
      D.el "br" pass
      D.text "User:"
      userNameInput <- textInput False user
      D.el "br" pass
      D.text "Password:"
      passwordInput <- textInput True password
      D.el "br" pass
      saveButton <- button "selector" $ D.text "Login"
      let saveEvent =
            liftA3
              (LoggedIn,,,)
              (inputValue urlInput)
              (inputValue userNameInput)
              (inputValue passwordInput)
              R.<@ fold (saveButton : (D.keypress D.Enter <$> [urlInput, userNameInput, passwordInput]))
      pure (saveEvent, saveEvent)
    LoggedIn -> do
      D.text [i|#{user} @ #{url}|]
      logoutEvent <-
        ((LoggedOut, url, user, password) <$)
          <$> button "selector" (D.text "Logout")
      pure (logoutEvent, logoutEvent)
  backendFromState (LoggedIn, url, user, password) = Just $ RemoteBackend url user password
  backendFromState (LoggedOut, _, _, _) = Nothing
  withBackend :: CloseEvent t -> Maybe (RemoteBackend Text) -> m (Maybe (StateProvider t m))
  withBackend innerCloseEvent = traverse (fmap makeStateProvider . webClientSocket innerCloseEvent)
  -- TODO: Get UI Config from Server
  textInput hidden defaultValue =
    D.inputElement $
      D.def
        & lensVL D.inputElementConfig_initialValue
        .~ defaultValue
        & lensVL (D.inputElementConfig_elementConfig . D.elementConfig_initialAttributes)
        .~ if hidden then "type" D.=: "password" else mempty
  inputValue = R.current . D._inputElement_value

data WebSocketState = WebSocketError Text | Connecting deriving stock (Show)

getStorage :: WidgetJSM t m => m Storage
getStorage = getLocalStorage =<< currentWindowUnchecked

webClientSocket ::
  WidgetJSM t m => CloseEvent t -> RemoteBackend Text -> m (ClientSocket t m)
webClientSocket closeEvent backend@RemoteBackend{url, user, password} = do
  refreshEvent <- button "selector" $ D.text "Refresh Tasks"
  let wsUrl = maybe "ws://localhost:8000" ("ws" <>) $ stripPrefix "http" url -- TODO: Warn user about missing http
      socketString = [i|#{wsUrl}/socket?username=#{user}&password=#{password}|]
      clientSocket socketRequestEvent = do
        let socketConfig =
              D.def
                & (lensVL D.webSocketConfig_send .~ (toList <$> (socketRequestEvent <> (one AllTasks <$ refreshEvent))))
                  . (lensVL D.webSocketConfig_reconnect .~ True)
                  . (lensVL D.webSocketConfig_close .~ ((3000, "Client unloaded websocket.") <$ un closeEvent))
        storage <- getStorage
        socket <- D.jsonWebSocket @SocketRequest @SocketMessage socketString socketConfig
        let messages = R.fmapMaybe id $ socket ^. lensVL D.webSocket_recv
            err = [i|Connection to kassandra server not possible. Check network connection, server url and credentials. #{backend}|]
            close = Just err <$ socket ^. lensVL D.webSocket_close
            open = Nothing <$ socket ^. lensVL D.webSocket_open
            messageParseFail = maybe (Just "Failed to parse SocketMessage JSON") (const Nothing) <$> socket ^. lensVL D.webSocket_recv
            nextStateEvent = R.leftmost [messageParseFail, close, open]
        D.dynText . fmap (fromMaybe "") =<< R.holdDyn (Just "Websocket Connecting ...") nextStateEvent
        let taskUpdates = (^? #_TaskUpdates) <$?> messages
            mapKey = [i|TaskMap#{wsUrl}#{user}|] :: String
            foldTasksToMap tasks currentMap = foldr (\task theMap -> insert (task ^. #uuid) task theMap) currentMap tasks
            asyncSaveStorage = void . liftJSM . D.forkJSM . setItem storage mapKey . decodeUtf8 @Text . encode
        taskMap :: Map UUID Task <- maybeToMonoid . (decode . encodeUtf8 @Text =<<) <$> getItem storage mapKey
        tasksToSave <- R.foldDyn foldTasksToMap taskMap taskUpdates
        -- TODO: Saving the whole Map everytime is probably quite inefficient, we should try to reduce writes by a smarter saving scheme
        void . R.performEventAsync $ const . asyncSaveStorage <$> R.updated tasksToSave
        ev <- R.getPostBuild
        let cachedTasks = R.fmapMaybe (nonEmptySeq . fromList) $ elems taskMap <$ ev
        pure . pure $ R.leftmost [messages, (#_TaskUpdates #) <$> cachedTasks]
  pure clientSocket
