{-# LANGUAGE DerivingStrategies #-}
module Kassandra.Standalone.Config
  ( readConfig
  , writeDeclarations
  , StandaloneConfig
  , StandaloneAccount(RemoteAccount, LocalAccount)
  , backends
  )
where

import           Dhall                          ( FromDhall )
import           Kassandra.Config               ( LocalBackend
                                                , NamedListQuery
                                                , PortConfig
                                                , RemoteBackend
                                                , TaskwarriorOption
                                                , TreeOption
                                                , UserConfig
                                                , Widget
                                                )
import           Kassandra.Config.Dhall         ( DhallLoadConfig(..)
                                                , dhallType
                                                , loadDhallConfig
                                                )
data StandaloneConfig = Config
  { backends :: Seq StandaloneBackend
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

data StandaloneBackend = StandaloneBackend
  { name    :: Text
  , account :: StandaloneAccount
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

data StandaloneAccount = RemoteAccount {backend :: RemoteBackend} | LocalAccount {userConfig :: UserConfig}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

dhallTypes :: Text
dhallTypes = [i|{
                 #{assignments}
               }|]
 where
  types :: [(String, Text)]
  types =
    [ ("StandaloneAccount", dhallType @StandaloneAccount)
    , ("Widget"           , dhallType @Widget)
    , ("NamedListQuery"   , dhallType @NamedListQuery)
    , ("LocalBackend"     , dhallType @LocalBackend)
    , ("TreeOption"       , dhallType @TreeOption)
    , ("PortConfig"       , dhallType @PortConfig)
    , ("TaskwarriorOption", dhallType @TaskwarriorOption)
    , ("StandaloneBackend", dhallType @StandaloneBackend)
    ]
  assignments =
    intercalate ",\n" $ (\(name, value) -> [i|#{name} = #{value}|]) <$> types

writeDeclarations :: IO ()
writeDeclarations =
  writeFileText "/home/maralorn/.config/kassandra/types.dhall" dhallTypes

readConfig :: Maybe Text -> IO StandaloneConfig
readConfig = loadDhallConfig DhallLoadConfig
  { envName = "KASSANDRA_CONFIG"
  , defaultFile = "~/.config/kassandra/config.dhall"
  , defaultConfig = [i|
    let
      types = #{dhallTypes} in
    {
      backends = [
        {
          name = "standardbackend",
          backend = types.StandaloneAccount.LocalAccount {
            userConfig = {
              localBackend = types.LocalBackend.TaskwarriorBackend {
                createHooksOnStart = True,
                hookListenPort = types.PortConfig.PortRange { min = 40000, max = 50000 },
                hookSuffix = "kassandra",
                removeHooksOnExit = True,
                taskBin = None Text,
                taskConfig = [] : List types.TaskwarriorOption,
                taskDataPath = None Text,
                taskRcPath = None Text
              },
              uiConfig = {
                configuredLists = [] : List types.NamedListQuery ,
                uiFeatures = {
                  sortInTag = False,
                  treeOption = types.TreeOption.NoTree
                },
                viewList = [ types.Widget.SearchWidget ]
              }
            }
          }
        }
      ]
    }
|]
  }
