module Common.Config
  ( BackendConfig
  , StandaloneConfig
  )
where

import           Data.Password.Argon2           ( PasswordHash
                                                , Argon2
                                                )

data BackendConfig = BackendConfig {
     users :: Map Text UserConfig
                                   } deriving (Show, Eq, Generic)

data StandaloneConfig = StandaloneConfig{
     backends :: Map Text Backend
                      } deriving (Show, Eq, Generic)

data UserConfig = UserConfig {
     passwordHash :: PasswordHash Argon2,
     backend :: Backend
                             } deriving (Show, Eq, Generic)

data PortConfig = Port Int | PortRange Int Int deriving (Show, Eq, Ord, Generic)

data PasswordConfig = Prompt | Password Text | PasswordCommand Text deriving (Show, Eq, Generic)

data Backend
  = LocalTaskwarriorBackend
      { -- | Set config file
        taskrcPath :: Maybe Text,
        -- | Set task data directory
        taskdataPath :: Maybe Text,
        -- | Override configrvariables
        taskconfig :: Map Text Text,
        -- | Path to taskwarrior binary. Nothing => Lookup "task" from PATH
        taskwarriorBin :: Maybe Text,
        -- | Use the first free port from the given range for the taskwarrior hook listener.
        hookListenPort :: PortConfig,
        -- | Created hooks are called ".on-add.<suffix>.<port>" and ".on-remove.<suffix>.<port>"
        hookSuffix :: Text,
        -- | Ensure existence of taskwarrior hook on every start
        createHooksOnStart :: Bool,
        -- | Remove hook on exit.
        removeHooksOnExit :: Bool
      }
  | LocalGitBackend
      { directoryPath :: Text,
        commit :: Bool,
        configureMerge :: Bool,
        createIfMissing :: Bool,
        origin :: Maybe Text,
        pushOnWrite :: Bool,
        watchFiles :: Bool,
        pullTimerSeconds :: Maybe Int
      }
  | RemoteBackend
      { url :: Text,
        user :: Text,
        password :: PasswordConfig
      }
 deriving (Show, Eq, Generic)
