{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances #-}
module Types
  ( Widget
  , ViewWidget
  , TaskInfos(..)
  , TaskState
  , StateChange(..)
  , StandardWidget
  , AppState(AppState)
  , getTime
  , getTasks
  , getAppState
  , HasAppState(appState)
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Data.List.NonEmpty             ( NonEmpty )
import           Control.Monad.Fix              ( MonadFix )
import           Data.HashMap.Strict            ( HashMap )
import           Data.UUID                      ( UUID )
import           ClassyPrelude
import           Taskwarrior.Task               ( Task )
import           Control.Monad.Reader           ( MonadReader )
import           Data.Time.LocalTime            ( ZonedTime )
import           Control.Lens.TH                ( makeClassy )
import           Control.Lens.Getter            ( (^.) )
type Widget t m
  = ( D.DomBuilder t m
    , D.DomBuilderSpace m ~ D.GhcjsDomSpace
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
    )
data TaskInfos = TaskInfos { task :: Task, showChildren :: Bool, children :: [UUID]} deriving (Eq, Show)

type TaskState = (HashMap UUID TaskInfos)

data StateChange = ToggleEvent UUID Bool | ChangeTask Task | CreateTask Text (Task -> Task)

data AppState t = AppState { _taskState :: R.Dynamic t TaskState, _currentTime :: R.Dynamic t ZonedTime }

makeClassy ''AppState

type ViewWidget t m r event
  = ( Widget t m
    , R.EventWriter t (NonEmpty event) m
    , MonadReader r m
    , HasAppState r t
    )

type StandardWidget t m r = ViewWidget t m r StateChange

getAppState :: (MonadReader r m, HasAppState r t) => m (AppState t)
getAppState = asks (^. appState)

getTime :: (MonadReader r m, HasAppState r t) => m (R.Dynamic t ZonedTime)
getTime = asks (^. currentTime)

getTasks :: (MonadReader r m, HasAppState r t) => m (R.Dynamic t TaskState)
getTasks = asks (^. taskState)
