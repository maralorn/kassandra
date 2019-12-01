module Types
  ( Widget
  , ViewWidget
  , TaskInfos(..)
  , TaskState
  , StateChange(..)
  , AppStateDyns(..)
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Control.Monad.Fix              ( MonadFix )
import           Data.HashMap.Strict            ( HashMap )
import           Data.UUID                      ( UUID )
import           ClassyPrelude
import           Taskwarrior.Task               ( Task )
import           Control.Monad.Reader           ( MonadReader )
import           Data.Time.LocalTime            ( ZonedTime )
type Widget t m
  = ( D.DomBuilder t m
    , D.DomBuilderSpace m ~ D.GhcjsDomSpace
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
    )
type ViewWidget t m event
  = (Widget t m, R.EventWriter t event m, MonadReader (AppStateDyns t) m)

data AppStateDyns t = AppStateDyns { getTaskState :: R.Dynamic t TaskState, getTime :: R.Dynamic t ZonedTime }

data TaskInfos = TaskInfos { task :: Task, showChildren :: Bool, children :: [UUID]} deriving (Eq, Show)

type TaskState = (HashMap UUID TaskInfos)

data StateChange = ToggleEvent (UUID, Bool) | ChangeTask Task deriving (Eq, Show)
