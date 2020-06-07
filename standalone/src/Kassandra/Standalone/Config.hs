module Kassandra.Standalone.Config
  ( readConfig, StandaloneConfig
  )
where

import           Dhall                          ( FromDhall )
import           Kassandra.Config.Dhall         ( DhallLoadConfig(..)
                                                , dhallType
                                                , loadDhallConfig
                                                )
import           Kassandra.Config               ( RemoteBackend
                                                , UserConfig
                                                )

data StandaloneConfig
  = Config
      { backends :: Seq StandaloneBackend
      }
  deriving (Show, Eq, Ord, Generic, FromDhall)

data StandaloneBackend
  = StandaloneBackend
      { name :: Text,
        account :: StandaloneAccount
      }
  deriving (Show, Eq, Ord, Generic, FromDhall)

data StandaloneAccount = RemoteAccount {backend :: RemoteBackend} | LocalAccount {userConfig :: UserConfig}
  deriving (Show, Eq, Ord, Generic, FromDhall)

readConfig :: Maybe Text -> IO StandaloneConfig
readConfig = loadDhallConfig DhallLoadConfig
  { envName       = "KASSANDRA_CONFIG"
  , defaultFile   = "~/.config/kassandra/config.dhall"
  , defaultConfig =
    "\\(types : { StandaloneAccount : Type }) -> { backends = [ { name = \"standardbackend\", account = types.StandaloneAccount.LocalAccount }] : "
    <> dhallType @(Seq StandaloneBackend)
    <> "}"
  }
