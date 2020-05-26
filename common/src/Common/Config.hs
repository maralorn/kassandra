module Common.Config
  ( AccountConfig
  , RemoteBackend
  , UserConfig
  , Dict
  )
where

import           Data.Password.Argon2           ( Argon2
                                                , PasswordHash(PasswordHash)
                                                )
import           Data.Sequence                  ( Seq )
import           Dhall                          ( Interpret
                                                , autoWith
                                                )
import qualified Dhall                         as Dhall
import qualified Data.UUID                     as UUID

instance Interpret (PasswordHash Argon2) where
  autoWith = fmap PasswordHash . autoWith

type Dict = Map Text

instance Interpret a => Interpret (Dict a) where
  autoWith = fmap fromList . autoWith

instance Interpret UUID where
  autoWith =
    (\x -> x { Dhall.extract = UUID.fromText <=< Dhall.extract x }) . autoWith

instance Interpret Word16 where
  autoWith =
    (\x -> x { Dhall.extract = integerToBounded <=< Dhall.extract x })
      . autoWith

data AccountConfig
  = AccountConfig
      { passwordHash :: PasswordHash Argon2,
        userConfig :: UserConfig,
        filterTag :: Text
      }
  deriving (Show, Eq, Ord, Generic, Interpret)

data UserConfig
  = UserConfig
      {
        localBackend :: LocalBackend,
        uiConfig :: UIConfig
      }
  deriving (Show, Eq, Ord, Generic, Interpret)

data UIConfig
  = UIConfig
      { viewList :: Seq Widget,
        configuredLists :: Dict ListQuery,
        sideView :: Seq Widget,
        uiFeatures :: UIFeatures
      }
  deriving (Show, Eq, Ord, Generic, Interpret)

data UIFeatures
  = UIFeatures
      { sortInTag :: Bool,
        treeOption :: TreeOption
      }
  deriving (Show, Eq, Ord, Generic, Interpret)

data Widget
  = SearchWidget
  | ListWidget ListQuery
  deriving (Show, Eq, Ord, Generic, Interpret)

data TreeOption = NoTree | PartOfTree | DependsTree
  deriving (Show, Eq, Ord, Generic, Interpret)


data ListItem
  = TaskwarriorTask UUID
  | AdHocTask Text
  | HabiticaTask HabiticaTask
  | Mail Text
  deriving (Show, Eq, Ord, Generic, Interpret)

data HabiticaTask = HabiticaDaily | HabiticaTodo
  deriving (Show, Eq, Ord, Generic, Interpret)

data HabiticaList = HabiticaDailys | HabiticaTodos
  deriving (Show, Eq, Ord, Generic, Interpret)

data DefinitionElement = SubList ListQuery (Maybe Natural) | ListElement ListItem
  deriving (Show, Eq, Ord, Generic, Interpret)

data ListQuery
  = QueryList Query
  | TagList Text
  | DefinitionList (Seq DefinitionElement)
  | ChildrenList UUID
  | DependenciesList UUID
  | ConfigList Text
  | HabiticaList HabiticaList
  | Mails
  deriving (Show, Eq, Ord, Generic, Interpret)

newtype Query = Seq QueryFilter deriving (Show, Eq, Ord, Generic, Interpret)

data QueryFilter = HasProperty TaskProperty | HasntProperty TaskProperty
  deriving (Show, Eq, Ord, Generic, Interpret)

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
  deriving (Show, Eq, Ord, Generic, Interpret)

data PortConfig = Port Word16 | PortRange Word16 Word16
  deriving (Show, Eq, Ord, Generic, Interpret)

data PasswordConfig = Prompt | Password Text | PasswordCommand Text
  deriving (Show, Eq, Ord, Generic, Interpret)

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
  deriving (Show, Eq, Ord, Generic, Interpret)
data RemoteBackend
  = RemoteBackend
      { url :: Text,
        user :: Text,
        password :: PasswordConfig
      }
  deriving (Show, Eq, Ord, Generic, Interpret)
