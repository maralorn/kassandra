module Kassandra.Standalone (
  standalone,
) where

import Control.Concurrent.STM (TQueue, newTQueueIO)
import Kassandra.Config (
  NamedBackend (NamedBackend, backend, name),
 )
import Kassandra.Css (cssAsBS)
import Kassandra.Debug (Severity (..), log, setLogLevel)
import Kassandra.LocalBackend (LocalBackendRequest)
import Kassandra.LocalBackendWidget (localBackendWidget)
import Kassandra.MainWidget (mainWidget)
import Kassandra.RemoteBackendWidget (CloseEvent (..), remoteBackendWidget)
import Kassandra.SelectorWidget (backendSelector)
import Kassandra.Standalone.Config (
  StandaloneAccount (LocalAccount, RemoteAccount),
  backends,
  readConfig,
  writeDeclarations,
 )
import Kassandra.Standalone.State (localBackendProvider)
import Kassandra.State (StateProvider)
import Kassandra.Types (WidgetJSM)
import Kassandra.Util (defDynDyn)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Newtype
import System.IO (hSetBuffering, BufferMode(..))
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(ExitFailure))

handleProvider :: IO a -> IO a
handleProvider m = catch m \(SomeException e) -> log Error [i|BackendProviderCrashed with error: #{e}|] >> exitImmediately (ExitFailure 1) >> error "Cannot be reached"
standalone :: IO ()
standalone = do
  hSetBuffering Prelude.stdout NoBuffering
  setLogLevel $ Just Info
  log Info "Started kassandra"
  log Debug "Writing Types file"
  writeDeclarations
  log Debug "Loading Config"
  config <- readConfig Nothing
  print config
  log Debug "Loaded Config"
  requestQueue <- newTQueueIO
  race_ (handleProvider (localBackendProvider requestQueue)) $ do
    log Info "Hanging in front of main widget"
    D.mainWidgetWithCss cssAsBS $ do
      log Info "Entered main widget"
      -- TODO: Use Config from stateProvider here
      D.dyn_ . (maybe pass (mainWidget D.def) <$>)
        =<< standaloneWidget requestQueue
        =<< backendSelector (backends config)

standaloneWidget ::
  WidgetJSM t m =>
  TQueue LocalBackendRequest ->
  R.Dynamic t (NamedBackend StandaloneAccount) ->
  m (R.Dynamic t (Maybe (StateProvider t m)))
standaloneWidget requestQueue accountDyn =
  defDynDyn (R.constDyn Nothing) $
    accountDyn
      <&> \NamedBackend{name, backend} -> case backend of
        RemoteAccount remoteAccount ->
          remoteBackendWidget
            (wrap $ () <$ R.updated accountDyn)
            remoteAccount
        LocalAccount localAccount ->
          localBackendWidget
            requestQueue
            NamedBackend{name, backend = localAccount}
