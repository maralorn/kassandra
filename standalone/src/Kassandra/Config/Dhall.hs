module Kassandra.Config.Dhall
  ( DhallLoadConfig(..)
  , loadDhallConfig
  , dhallType
  )
where

import           Dhall.Core                     ( pretty )
import           Dhall                          ( auto
                                                , input
                                                , expected
                                                , FromDhall
                                                , autoWith
                                                , toMonadic
                                                , fromMonadic
                                                , extractError
                                                , Decoder
                                                )
import qualified Dhall
import           Data.Password.Argon2           ( Argon2
                                                , PasswordHash(PasswordHash)
                                                )
import qualified Data.UUID                     as UUID


import           Kassandra.Config               ( AccountConfig
                                                , UserConfig
                                                , LocalBackend
                                                , PortConfig
                                                , UIConfig
                                                , Widget
                                                , ListQuery
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
                                                , NamedListQuery
                                                )


instance FromDhall PasswordConfig
instance FromDhall RemoteBackend
instance FromDhall TreeOption
instance FromDhall HabiticaTask
instance FromDhall ListItem
instance FromDhall DefinitionElement
instance FromDhall TaskProperty
instance FromDhall QueryFilter
instance FromDhall HabiticaList
instance FromDhall ListQuery
instance FromDhall Widget
instance FromDhall UIFeatures
instance FromDhall UIConfig
instance FromDhall NamedListQuery
instance FromDhall PortConfig
instance FromDhall LocalBackend
instance FromDhall UserConfig
instance FromDhall AccountConfig

postComposeMayDecoder :: Text -> (a -> Maybe b) -> Decoder a -> Decoder b
postComposeMayDecoder err f dec = dec
  { Dhall.extract = fromMonadic
                      . (   maybe (toMonadic $ extractError err) Right
                        .   f
                        <=< toMonadic
                        .   Dhall.extract dec
                        )
  }
instance FromDhall UUID where
  autoWith =
    postComposeMayDecoder "Text was on valid UUID" UUID.fromText . autoWith

instance FromDhall Word16 where
  autoWith =
    postComposeMayDecoder "Integer was not a Word16" integerToBounded . autoWith

instance FromDhall (PasswordHash Argon2) where
  autoWith = fmap PasswordHash . autoWith

data DhallLoadConfig
  = DhallLoadConfig
      { envName :: Text,
        defaultFile :: Text,
        defaultConfig :: Text
      }
  deriving (Show, Eq, Ord)

dhallType :: forall a . FromDhall a => Text
dhallType = pretty . expected $ auto @a

loadDhallConfig :: FromDhall a => DhallLoadConfig -> Maybe Text -> IO a
loadDhallConfig loadConfig configFile = input
  auto
  (defaultConfig loadConfig <> " // " <> config)
 where
  config = fromMaybe
    (  "((env:"
    <> envName loadConfig
    <> " ? "
    <> defaultFile loadConfig
    <> ") ? { = })"
    )
    configFile
