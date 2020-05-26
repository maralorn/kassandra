module Backend.Config
  ( readConfig
  , DhallLoadConfig(..)
  , loadDhallConfig
  , Config
  )
where

import           Dhall                          ( auto
                                                , input
                                                , Interpret
                                                )
import           Common.Config                  ( AccountConfig )

data Config
  = Config
      { users :: Map Text AccountConfig
      }
  deriving (Show, Eq, Generic)

instance Interpret Config

readConfig :: Maybe Text -> IO Config
readConfig = loadDhallConfig DhallLoadConfig
  { envName       = "KASSANDRA_BACKEND_CONFIG"
  , defaultFile   = "~/.config/kassandra/backend.dhall"
  , defaultConfig = "{ }"
  }

data DhallLoadConfig
  = DhallLoadConfig
      { envName :: Text,
        defaultFile :: Text,
        defaultConfig :: Text
      }
  deriving (Show, Eq, Ord)

loadDhallConfig :: Interpret a => DhallLoadConfig -> Maybe Text -> IO a
loadDhallConfig loadConfig configFile = input
  auto
  (defaultConfig loadConfig <> " // " <> config)
 where
  config = fromMaybe
    ("((env:" <> envName loadConfig <> " ? " <> defaultFile loadConfig <> ") ? { })")
    configFile
