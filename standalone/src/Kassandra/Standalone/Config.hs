{-# LANGUAGE DerivingStrategies #-}
module Kassandra.Standalone.Config
  ( readConfig
  , writeDeclarations
  , StandaloneConfig
  )
where

import           Dhall                          ( FromDhall )
import           Kassandra.Config.Dhall         ( DhallLoadConfig(..)
                                                , dhallType
                                                , loadDhallConfig
                                                )
import           Kassandra.Config               ( RemoteBackend
                                                , UserConfig
                                                , Widget
                                                , TreeOption
                                                , NamedListQuery
                                                , LocalBackend
                                                , PortConfig
                                                , TaskwarriorOption
                                                )

data StandaloneConfig
  = Config
      { backends :: Seq StandaloneBackend
      }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

data StandaloneBackend
  = StandaloneBackend
      { name :: Text,
        account :: StandaloneAccount
      }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

data StandaloneAccount = RemoteAccount {backend :: RemoteBackend} | LocalAccount {userConfig :: UserConfig}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass FromDhall

dhallTypes :: Text
dhallTypes = [i|
  {
    StandaloneBackend = #{dhallType @StandaloneBackend},
    StandaloneAccount = #{dhallType @StandaloneAccount},
    Widget = #{dhallType @Widget},
    NamedListQuery = #{dhallType @NamedListQuery},
    LocalBackend = #{dhallType @LocalBackend},
    TreeOption = #{dhallType @TreeOption},
    PortConfig = #{dhallType @PortConfig},
    TaskwarriorOption = #{dhallType @TaskwarriorOption}
  }
|]

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
          account = types.StandaloneAccount.LocalAccount {
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
