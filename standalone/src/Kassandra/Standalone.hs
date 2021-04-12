module Kassandra.Standalone (
  standalone,
) where

import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled))
import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Exception (throwIO)
import Data.Typeable (typeOf)
import Kassandra.Config (NamedBackend (NamedBackend, backend, name))
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
import Relude.Extra.Newtype (wrap)
import System.Exit (ExitCode (ExitFailure))
import System.IO (BufferMode (..), hSetBuffering)
import System.Posix.Process (exitImmediately)

-- | This function worksaround the fact that a reflex mainWidget does not exit when it catches an Async Exception.
exitImmediatelyOnLocalException :: IO a -> IO a
exitImmediatelyOnLocalException m = catch m catcher
 where
  catcher (SomeException e) = do
    when (typeOf e /= typeOf AsyncCancelled) do
      log Error [i|BackendProviderCrashed with error: #{e}|]
      exitImmediately (ExitFailure 1)
    throwIO e

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
  race_ (exitImmediatelyOnLocalException (localBackendProvider requestQueue)) do
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
