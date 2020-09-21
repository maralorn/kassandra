module Kassandra.LocalBackendWidget
  ( localBackendWidget
  )
where

import           Control.Concurrent.STM         ( TQueue )
import           Kassandra.LocalBackend         ( LocalBackendRequest, localClientSocket )
import           Kassandra.Config               ( NamedBackend
                                                  ( NamedBackend
                                                  , name
                                                  , backend
                                                  )
                                                , UserConfig(UserConfig, localBackend, uiConfig)
                                                )
import           Kassandra.State                ( AppContext, makeStateProvider )
import           Kassandra.Types                ( WidgetIO )
import qualified Reflex                        as R

localBackendWidget
  :: WidgetIO t m
  => TQueue LocalBackendRequest
  -> NamedBackend UserConfig
  -> m (R.Dynamic t (Maybe (AppContext t m)))
localBackendWidget requestsQueue (NamedBackend { name, backend = UserConfig { localBackend, uiConfig } })
  = do
    clientSocket <- localClientSocket requestsQueue localBackend :: _
    let stateProvider = makeStateProvider clientSocket
    undefined :: _
