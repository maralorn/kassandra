module Kassandra.LocalBackendWidget (
  localBackendWidget,
) where

import Control.Concurrent.STM (TQueue)
import Kassandra.Config (
  NamedBackend (NamedBackend, backend),
  UserConfig (UserConfig, localBackend, uiConfig),
 )
import Kassandra.LocalBackend (
  LocalBackendRequest,
  localClientSocket,
 )
import Kassandra.State (
  StateProvider,
  makeStateProvider,
 )
import Kassandra.Types (WidgetIO)
import qualified Reflex as R

localBackendWidget ::
  WidgetIO t m =>
  TQueue LocalBackendRequest ->
  NamedBackend UserConfig ->
  m (R.Dynamic t (Maybe (StateProvider t m)))
localBackendWidget requestsQueue NamedBackend{backend = UserConfig{localBackend, uiConfig}} =
  -- TODO: Use the uiConfig
  pure . pure . makeStateProvider <$> localClientSocket requestsQueue localBackend
