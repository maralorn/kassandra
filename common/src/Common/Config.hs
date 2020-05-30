module Common.Config
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
  )
where

import           Data.Password.Argon2           ( Argon2
                                                , PasswordHash
                                                )
import           Data.Sequence                  ( Seq )

type Dict = Map Text

data AccountConfig
  = AccountConfig
      { passwordHash :: PasswordHash Argon2,
        userConfig :: UserConfig,
        filterTag :: Text
      }
  deriving (Show, Eq, Ord, Generic)

data UserConfig
  = UserConfig
      {
        localBackend :: LocalBackend,
        uiConfig :: UIConfig
      }
  deriving (Show, Eq, Ord, Generic)

data UIConfig
  = UIConfig
      { viewList :: Seq Widget,
        configuredLists :: Dict ListQuery,
        sideView :: Seq Widget,
        uiFeatures :: UIFeatures
      }
  deriving (Show, Eq, Ord, Generic)

data UIFeatures
  = UIFeatures
      { sortInTag :: Bool,
        treeOption :: TreeOption
      }
  deriving (Show, Eq, Ord, Generic)

data Widget
  = SearchWidget
  | ListWidget ListQuery
  deriving (Show, Eq, Ord, Generic)

data TreeOption = NoTree | PartOfTree | DependsTree
  deriving (Show, Eq, Ord, Generic)


data ListItem
  = TaskwarriorTask UUID
  | AdHocTask Text
  | HabiticaTask HabiticaTask
  | Mail Text
  deriving (Show, Eq, Ord, Generic)

data HabiticaTask = HabiticaDaily | HabiticaTodo
  deriving (Show, Eq, Ord, Generic)

data HabiticaList = HabiticaDailys | HabiticaTodos
  deriving (Show, Eq, Ord, Generic)

data DefinitionElement = SubList ListQuery (Maybe Natural) | ListElement ListItem
  deriving (Show, Eq, Ord, Generic)

data ListQuery
  = QueryList Query
  | TagList Text
  | DefinitionList (Seq DefinitionElement)
  | ChildrenList UUID
  | DependenciesList UUID
  | ConfigList Text
  | HabiticaList HabiticaList
  | Mails
  deriving (Show, Eq, Ord, Generic)

newtype Query = Seq QueryFilter deriving (Show, Eq, Ord, Generic)

data QueryFilter = HasProperty TaskProperty | HasntProperty TaskProperty
  deriving (Show, Eq, Ord, Generic)

data TaskProperty
  = DescriptionMatches Text
  | ParentBlocked
  | Blocked
  | Waiting
  | Pending
  | Completed
  | Deleted
  | IsParent
  | OnList
  | HasTag Text
  | HasParent
  deriving (Show, Eq, Ord, Generic)

data PortConfig = Port Word16 | PortRange Word16 Word16
  deriving (Show, Eq, Ord, Generic)

data PasswordConfig = Prompt | Password Text | PasswordCommand Text
  deriving (Show, Eq, Ord, Generic)

data LocalBackend
  = TaskwarriorBackend
      { -- | Set config file
        taskRcPath :: Maybe Text,
        -- | Set task data directory
        taskDataPath :: Maybe Text,
        -- | Override config variables
        taskConfig :: Dict Text,
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
  deriving (Show, Eq, Ord, Generic)
data RemoteBackend
  = RemoteBackend
      { url :: Text,
        user :: Text,
        password :: PasswordConfig
      }
  deriving (Show, Eq, Ord, Generic)
