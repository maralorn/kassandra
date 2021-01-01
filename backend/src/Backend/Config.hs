module Backend.Config
  ( readConfig
  , BackendConfig(..)
  ) where

import           Dhall                          ( FromDhall )
import           Kassandra.Config               ( AccountConfig
                                                , Dict
                                                )
import           Kassandra.Config.Dhall         ( DhallLoadConfig
                                                  ( DhallLoadConfig
                                                  )
                                                , defaultConfig
                                                , defaultFile
                                                , envName
                                                , loadDhallConfig
                                                )

data BackendConfig = BackendConfig
  { users :: Dict AccountConfig
  }
  deriving (Show, Eq, Generic, FromDhall)

readConfig :: Maybe Text -> IO BackendConfig
readConfig = loadDhallConfig DhallLoadConfig
  { envName       = "KASSANDRA_BACKEND_CONFIG"
  , defaultFile   = "~/.config/kassandra/backend.dhall"
  , defaultConfig = "{ = }"
  }
