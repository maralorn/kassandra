module Standalone.Config
  ( readConfig
  , Config
  )
where

import           Dhall                          ( Interpret )
import           Backend.Config                 ( loadDhallConfig
                                                , DhallLoadConfig(..)
                                                )
import           Common.Config                  ( RemoteBackend
                                                , UserConfig
                                                , Dict
                                                )

data Config
  = Config
      { backends :: Dict StandaloneAccount
      }
  deriving (Show, Eq, Ord, Generic, Interpret)

data StandaloneAccount = RemoteAccount RemoteBackend | LocalAccount UserConfig
  deriving (Show, Eq, Ord, Generic, Interpret)

readConfig :: Maybe Text -> IO Config
readConfig = loadDhallConfig DhallLoadConfig
  { envName       = "KASSANDRA_CONFIG"
  , defaultFile   = "~/.config/kassandra/config.dhall"
  , defaultConfig = "{}"
  }
