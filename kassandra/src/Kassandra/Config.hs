{-# LANGUAGE DerivingStrategies #-}
module Kassandra.Config
  ( AccountConfig
  , RemoteBackend
  , UserConfig
  , LocalBackend
  , Dict
  , UIConfig
  , PortConfig
  , Widget
  , TreeOption
  , ListItem
  , HabiticaTask
  , HabiticaList
  , DefinitionElement
  , ListQuery
  , Query
  , QueryFilter
  , TaskProperty
  , UIFeatures
  , PasswordConfig
  , NamedListQuery
  , TaskwarriorOption
  )
where

import           Data.Password.Argon2           ( Argon2
                                                , PasswordHash
                                                )

type Dict = Map Text


data AccountConfig
  = AccountConfig
      { passwordHash :: PasswordHash Argon2,
        userConfig :: UserConfig,
        filterTag :: Text
      }
  deriving stock (Show, Eq, Ord, Generic)

data UserConfig
  = UserConfig
      { localBackend :: LocalBackend,
        uiConfig :: UIConfig
      }
  deriving stock (Show, Eq, Ord, Generic)

data UIConfig
  = UIConfig
      { viewList :: Seq Widget,
        configuredLists :: Seq NamedListQuery,
        uiFeatures :: UIFeatures
      }
  deriving stock (Show, Eq, Ord, Generic)

data UIFeatures
  = UIFeatures
      { sortInTag :: Bool,
        treeOption :: TreeOption
      }
  deriving stock (Show, Eq, Ord, Generic)

data Widget
  = SearchWidget
  | ListWidget {query :: ListQuery}
  deriving stock (Show, Eq, Ord, Generic)

data TreeOption = NoTree | PartOfTree | DependsTree
  deriving stock (Show, Eq, Ord, Generic)

data ListItem
  = TaskwarriorTask {uuid :: UUID}
  | AdHocTask {description :: Text}
  | HabiticaTask {task :: HabiticaTask}
  | Mail {id :: Text}
  deriving stock (Show, Eq, Ord, Generic)

data HabiticaTask = HabiticaDaily | HabiticaTodo
  deriving stock (Show, Eq, Ord, Generic)

data HabiticaList = HabiticaDailys | HabiticaTodos
  deriving stock (Show, Eq, Ord, Generic)

data DefinitionElement = ConfigList {name :: Text, limit :: Maybe Natural} | ListElement {item :: ListItem}
  deriving stock (Show, Eq, Ord, Generic)

data TaskwarriorOption = TaskwarriorOption {name :: Text, value :: Text}
  deriving stock (Show, Eq, Ord, Generic)

data NamedListQuery
  = NamedListQuery
      { name :: Text,
        list :: ListQuery
      }
  deriving stock (Show, Eq, Ord, Generic)

data ListQuery
  = QueryList {query :: Query}
  | TagList {name :: Text}
  | DefinitionList {elements :: Seq DefinitionElement}
  | ChildrenList {uuid :: UUID}
  | DependenciesList {uuid :: UUID}
  | HabiticaList {list :: HabiticaList}
  | Mails
  deriving stock (Show, Eq, Ord, Generic)

type Query = Seq QueryFilter

data QueryFilter = HasProperty {property :: TaskProperty} | HasntProperty {property :: TaskProperty}
  deriving stock (Show, Eq, Ord, Generic)

data TaskProperty
  = DescriptionMatches {filter :: Text}
  | ParentBlocked
  | Blocked
  | Waiting
  | Pending
  | Completed
  | Deleted
  | IsParent
  | OnList
  | HasTag {tag :: Text}
  | HasParent
  deriving stock (Show, Eq, Ord, Generic)

data PortConfig = Port {port :: Word16} | PortRange {min :: Word16, max :: Word16}
  deriving stock (Show, Eq, Ord, Generic)

data PasswordConfig = Prompt | Password {plaintext :: Text} | PasswordCommand {command :: Text}
  deriving stock (Show, Eq, Ord, Generic)

data LocalBackend
  = TaskwarriorBackend
      { -- | Set config file
        taskRcPath :: Maybe Text,
        -- | Set task data directory
        taskDataPath :: Maybe Text,
        -- | Override config variables
        taskConfig :: Seq TaskwarriorOption,
        -- | Path to taskwarrior binary. Nothing => Lookup "task" from PATH
        taskBin :: Maybe Text,
        -- | Use the first free port from the given range for the taskwarrior hook listener.
        hookListenPort :: PortConfig,
        -- | Created hooks are called ".on-add.<suffix>.<port>" and ".on-remove.<suffix>.<port>"
        hookSuffix :: Text,
        -- | Ensure existence of taskwarrior hook on every start
        createHooksOnStart :: Bool,
        -- | Remove hook on exit.
        removeHooksOnExit :: Bool
      }
  | GitBackend
      { directoryPath :: Text,
        commit :: Bool,
        configureMerge :: Bool,
        createIfMissing :: Bool,
        origin :: Maybe Text,
        pushOnWrite :: Bool,
        watchFiles :: Bool,
        pullTimerSeconds :: Maybe Natural
      }
  deriving stock (Show, Eq, Ord, Generic)

data RemoteBackend
  = RemoteBackend
      { url :: Text,
        user :: Text,
        password :: PasswordConfig
      }
  deriving stock (Show, Eq, Ord, Generic)
