module Kassandra.Standalone
  ( standalone
  ) where

import           Control.Concurrent.STM         ( TQueue
                                                , newTQueue
                                                )
import           Kassandra.Config               ( NamedBackend
                                                  ( NamedBackend
                                                  , backend
                                                  , name
                                                  )
                                                )
import           Kassandra.Css                  ( cssAsBS )
import           Kassandra.Debug                ( Severity(..)
                                                , log
                                                , setLogLevel
                                                )
import           Kassandra.LocalBackend         ( LocalBackendRequest )
import           Kassandra.LocalBackendWidget   ( localBackendWidget )
import           Kassandra.MainWidget           ( mainWidget )
import           Kassandra.RemoteBackendWidget  ( remoteBackendWidget )
import           Kassandra.SelectorWidget       ( backendSelector )
import           Kassandra.Standalone.Config    ( StandaloneAccount
                                                  ( LocalAccount
                                                  , RemoteAccount
                                                  )
                                                , backends
                                                , readConfig
                                                , writeDeclarations
                                                )
import           Kassandra.Standalone.State     ( localBackendProvider )
import           Kassandra.State                ( AppContext )
import           Kassandra.Types                ( WidgetJSM )
import           Kassandra.Util                 ( defDynDyn )
import qualified Reflex                        as R
import qualified Reflex.Dom                    as D


standalone :: IO ()
standalone = do
  setLogLevel $ Just Debug
  log Info  "Started kassandra"
  log Debug "Writing Types file"
  writeDeclarations
  log Debug "Loading Config"
  config <- readConfig Nothing
  print config
  log Debug "Loaded Config"
  requestQueue <- atomically newTQueue
  race_ (localBackendProvider requestQueue)
    $   D.mainWidgetWithCss cssAsBS
    $   D.dyn_
    . (maybe pass
             (\(stateProvider, uiConfig) -> mainWidget uiConfig stateProvider) <$>
      )
    =<< standaloneWidget requestQueue
    =<< backendSelector (backends config)

standaloneWidget
  :: WidgetJSM t m
  => TQueue LocalBackendRequest
  -> R.Dynamic t (NamedBackend StandaloneAccount)
  -> m (R.Dynamic t (Maybe (AppContext t m)))
standaloneWidget requestQueue accountDyn =
  defDynDyn (R.constDyn Nothing)
    $   accountDyn
    <&> \NamedBackend { name, backend } -> case backend of
          RemoteAccount remoteAccount -> remoteBackendWidget (CloseEvent $ () <$ R.updated accountDyn) remoteAccount
          LocalAccount  localAccount  -> localBackendWidget
            requestQueue
            NamedBackend { name, backend = localAccount }
