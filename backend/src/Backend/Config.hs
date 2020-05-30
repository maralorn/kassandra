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
                                                , autoWith
                                                )
import           Data.Password.Argon2           ( Argon2
                                                , PasswordHash(PasswordHash)
                                                )
import qualified Data.UUID                     as UUID
import qualified Dhall
import           Common.Config                  ( AccountConfig
                                                , Dict
                                                , UserConfig
                                                , LocalBackend
                                                , PortConfig
                                                , UIConfig
                                                , Widget
                                                , ListQuery
                                                , Query
                                                , UIFeatures
                                                , TreeOption
                                                , DefinitionElement
                                                , QueryFilter
                                                , HabiticaList
                                                , HabiticaTask
                                                , TaskProperty
                                                , ListItem
                                                , PasswordConfig
                                                , RemoteBackend
                                                )

data Config
  = Config
      { users :: Map Text AccountConfig
      }
  deriving (Show, Eq, Generic)


instance Interpret PasswordConfig
instance Interpret RemoteBackend
instance Interpret TreeOption
instance Interpret HabiticaTask
instance Interpret ListItem
instance Interpret DefinitionElement
instance Interpret Query
instance Interpret TaskProperty
instance Interpret QueryFilter
instance Interpret HabiticaList
instance Interpret ListQuery
instance Interpret Widget
instance Interpret UIFeatures
instance Interpret UIConfig
instance Interpret PortConfig
instance Interpret LocalBackend
instance Interpret UserConfig
instance Interpret AccountConfig
instance Interpret Config
instance Interpret a => Interpret (Dict a) where
  autoWith = fmap fromList . autoWith

instance Interpret UUID where
  autoWith =
    (\x -> x { Dhall.extract = UUID.fromText <=< Dhall.extract x }) . autoWith

instance Interpret Word16 where
  autoWith =
    (\x -> x { Dhall.extract = integerToBounded <=< Dhall.extract x })
      . autoWith

instance Interpret (PasswordHash Argon2) where
  autoWith = fmap PasswordHash . autoWith

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
    (  "((env:"
    <> envName loadConfig
    <> " ? "
    <> defaultFile loadConfig
    <> ") ? { })"
    )
    configFile
