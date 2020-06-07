module Backend.Config
  ( readConfig
  )
where

import           Dhall                          ( FromDhall )
import           Kassandra.Config               ( Dict
                                                , AccountConfig
                                                )
import           Kassandra.Config.Dhall         ( DhallLoadConfig
                                                  ( DhallLoadConfig
                                                  )
                                                , envName
                                                , defaultFile
                                                , defaultConfig
                                                , loadDhallConfig
                                                )

data BackendConfig
  = BackendConfig
      { users :: Dict AccountConfig
      }
  deriving (Show, Eq, Generic, FromDhall)

readConfig :: Maybe Text -> IO BackendConfig
readConfig = loadDhallConfig DhallLoadConfig
  { envName       = "KASSANDRA_BACKEND_CONFIG"
  , defaultFile   = "~/.config/kassandra/backend.dhall"
  , defaultConfig = "{ = }"
  }
