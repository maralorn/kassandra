module Kassandra.LocalBackendWidget
  ( localBackendWidget
  ) where

import           Control.Concurrent.STM         ( TQueue )
import           Kassandra.Config               ( NamedBackend
                                                  ( NamedBackend
                                                  , backend
                                                  )
                                                , UserConfig
                                                  ( UserConfig
                                                  , localBackend
                                                  , uiConfig
                                                  )
                                                )
import           Kassandra.LocalBackend         ( LocalBackendRequest
                                                , localClientSocket
                                                )
import           Kassandra.State                ( AppContext
                                                , makeStateProvider
                                                )
import           Kassandra.Types                ( WidgetIO )
import qualified Reflex                        as R

localBackendWidget
  :: WidgetIO t m
  => TQueue LocalBackendRequest
  -> NamedBackend UserConfig
  -> m (R.Dynamic t (Maybe (AppContext t m)))
localBackendWidget requestsQueue NamedBackend { backend = UserConfig { localBackend, uiConfig } }
  = do
    clientSocket <- localClientSocket requestsQueue localBackend
    let stateProvider = makeStateProvider clientSocket
    pure $ pure $ Just (stateProvider, uiConfig)
