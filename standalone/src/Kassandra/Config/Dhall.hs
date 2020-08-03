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
import           System.Environment             ( lookupEnv )


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
                                                , TaskwarriorOption
                                                )
import           System.Path.IO                 ( FsPath(FsPath)
                                                , doesFileExist
                                                , fromFilePath
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
instance FromDhall TaskwarriorOption
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
    postComposeMayDecoder "Text was no valid UUID" UUID.fromText . autoWith

instance FromDhall Word16 where
  autoWith =
    postComposeMayDecoder "Natural was not a Word16"
                          (integerToBounded . toInteger @Natural)
      . autoWith

instance FromDhall (PasswordHash Argon2) where
  autoWith = fmap PasswordHash . autoWith

data DhallLoadConfig
  = DhallLoadConfig
      { envName :: Text,
        defaultFile :: Text,
        defaultConfig :: Text
      }
  deriving stock (Show, Eq, Ord)

dhallType :: forall a . FromDhall a => Text
dhallType = pretty . expected $ auto @a

loadDhallConfig :: FromDhall a => DhallLoadConfig -> Maybe Text -> IO a
loadDhallConfig loadConfig givenConfigFile = do
  let defFile = defaultFile loadConfig
      defConf = defaultConfig loadConfig
  filename <- firstJustM
    [ pure givenConfigFile
    , fmap toText <$> lookupEnv (toString $ envName loadConfig)
    , doesPathExist defFile
      <&> \doesExist -> if doesExist then Just defFile else Nothing
    ]
  input auto $ maybe defConf (\name -> [i|(#{defConf}) // #{name}|]) filename

doesPathExist :: ToString a => a -> IO Bool
doesPathExist (fromFilePath . toString -> (FsPath path)) = doesFileExist path
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM []       = pure Nothing
firstJustM (a : as) = a >>= \x -> if isJust x then pure x else firstJustM as
