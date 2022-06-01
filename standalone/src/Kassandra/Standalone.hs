module Kassandra.Standalone (
  standalone,
) where

import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Exception (SomeAsyncException, throwIO)
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
  dhallTypes,
  readConfig,
 )
import Kassandra.Standalone.State (localBackendProvider)
import Kassandra.State (StateProvider)
import Kassandra.Types (WidgetJSM)
import Kassandra.Util (defDynDyn)
import qualified Reflex as R
import qualified Reflex.Dom as D
import Relude.Extra.Newtype (wrap)
import Say (say)
import System.Exit (ExitCode (ExitFailure))
import System.Posix.Process (exitImmediately)

-- | This function works around the fact that a reflex mainWidget does not exit when it catches an Async Exception.
exitImmediatelyOnLocalException :: IO a -> IO a
exitImmediatelyOnLocalException m = catch m catcher
 where
  catcher outer@(SomeException inner) = do
    when (isNothing (fromException outer :: Maybe SomeAsyncException)) do
      log Error [i|BackendProviderCrashed with error: #{inner} with type #{typeOf inner}|]
      exitImmediately (ExitFailure 1)
    throwIO inner

standalone :: IO ()
standalone = do
  args <- getArgs
  when (args == ["print-types"]) do
    say dhallTypes
    exitSuccess
  hSetBuffering Prelude.stdout NoBuffering
  setLogLevel $ Just Info
  log Info "Started kassandra"
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
      D.dyn_ . (maybe pass mainWidget <$>)
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
            (wrap $ void $ R.updated accountDyn)
            remoteAccount
        LocalAccount localAccount ->
          localBackendWidget
            requestQueue
            NamedBackend{name, backend = localAccount}
