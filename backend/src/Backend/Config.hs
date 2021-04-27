module Backend.Config (
  readConfig,
  BackendConfig (..),
) where

import Dhall (FromDhall)
import Kassandra.Config (
  AccountConfig,
  Dict,
 )
import Kassandra.Config.Dhall (
  DhallLoadConfig (
    DhallLoadConfig
  ),
  defaultConfig,
  defaultFile,
  envName,
  loadDhallConfig,
 )
import Kassandra.Standalone.Config (
  BackendConfig (..),
 )

readConfig :: Maybe Text -> IO BackendConfig
readConfig =
  loadDhallConfig
    DhallLoadConfig
      { envName = "KASSANDRA_BACKEND_CONFIG"
      , defaultFile = "~/.config/kassandra/backend.dhall"
      , defaultConfig = "{ = }"
      }
