module Kassandra.Types (
  Widget,
  WidgetIO,
  WidgetJSM,
  TaskInfos (..),
  TaskState,
  AppStateChange,
  DragState (DraggedTasks, NoDrag),
  TaskTreeState,
  TaskTreeStateChange,
  DataChange (ChangeTask, CreateTask),
  FilterState (FilterState),
  StandardWidget,
  TaskTreeWidget,
  ExpandedTasks,
  ToggleEvent (ToggleEvent),
  WriteApp,
  Write,
  Have,
  HaveApp,
  AppState (AppState),
  getAppState,
  getTasks,
  getDragState,
  getTime,
  getIsExpanded,
  getExpandedTasks,
) where

import qualified Data.Aeson as Aeson
import Data.HashSet (member)
import Language.Javascript.JSaddle (MonadJSM)
import qualified Reflex as R
import qualified Reflex.Dom as D
import qualified Taskwarrior.Status
import qualified Taskwarrior.Task
import Kassandra.Calendar

type Widget t m =
  ( D.DomBuilder t m
  , D.DomBuilderSpace m ~ D.GhcjsDomSpace
  , MonadFix m
  , R.MonadHold t m
  , R.PostBuild t m
  , MonadIO m
  , R.TriggerEvent t m
  , R.PerformEvent t m
  , MonadIO (R.Performable m)
  , HasCallStack
  )
type WidgetJSM t m =
  (D.HasJSContext m, MonadJSM (R.Performable m), MonadJSM m, WidgetIO t m)
type WidgetIO t m = Widget t m
data TaskInfos = TaskInfos {task :: Task, children :: [UUID], parents :: [UUID], revDepends :: [UUID], blocked :: Bool} deriving stock (Eq, Show, Generic)
makeLabels ''TaskInfos

type TaskState = (HashMap UUID TaskInfos)

data DragState = DraggedTasks (NonEmpty UUID) | DraggedTag Text | NoDrag deriving stock (Eq, Show, Generic)
makePrismLabels ''DragState

data DataChange = ChangeTask Task | CreateTask Text (Task -> Task) deriving stock (Generic)
makePrismLabels ''DataChange

data ToggleEvent = ToggleEvent UUID Bool deriving stock (Eq, Show, Generic)
makePrismLabels ''ToggleEvent

type AppStateChange = Either DragState DataChange
type TaskTreeStateChange = Either AppStateChange ToggleEvent

instance {-# OVERLAPPING #-} AsType AppStateChange AppStateChange where
  _Typed = castOptic equality
instance {-# OVERLAPPING #-} AsType TaskTreeStateChange TaskTreeStateChange where
  _Typed = castOptic equality

data FilterState = FilterState {deletedFade :: NominalDiffTime, completedFade :: NominalDiffTime} deriving stock (Eq, Show, Generic)
makeLabels ''FilterState

type ExpandedTasks = HashSet UUID
type TaskTreeState t = R.Dynamic t ExpandedTasks

data AppState t = AppState {taskState :: R.Dynamic t TaskState, currentTime :: R.Dynamic t ZonedTime, dragState :: R.Dynamic t DragState, calendarEvents :: R.Dynamic t (Seq CalendarEvent)} deriving stock (Generic)
makeLabels ''AppState

type Have m r s = (MonadReader r m, HasType s r)
type HaveApp t m r = (R.Reflex t, Have m r (AppState t))
type HaveTaskTree t m r = (Have m r (TaskTreeState t))
type Write t m e s = (R.Reflex t, R.EventWriter t (NonEmpty e) m, AsType s e)
type WriteApp t m e = (Write t m e AppStateChange)
type WriteTaskTree t m e = (Write t m e TaskTreeStateChange)
type StandardWidget t m r e = (Widget t m, HaveApp t m r, WriteApp t m e)
type TaskTreeWidget t m r e = (StandardWidget t m r e, HaveTaskTree t m r, WriteTaskTree t m e)

getIsExpanded ::
  (Widget t m, HaveTaskTree t m r) => UUID -> m (R.Dynamic t Bool)
getIsExpanded uuid = R.holdUniqDyn . fmap (member uuid) =<< getExpandedTasks

getExpandedTasks :: HaveTaskTree t m r => m (TaskTreeState t)
getExpandedTasks = asks (^. typed)

getAppState :: (MonadReader r m, HasType (AppState t) r) => m (AppState t)
getAppState = asks (^. typed)
getTasks ::
  (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t TaskState)
getTasks = getAppState ^. mapping #taskState
getDragState ::
  (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t DragState)
getDragState = getAppState ^. mapping #dragState
getTime ::
  (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t ZonedTime)
getTime = getAppState ^. mapping #currentTime

deriving stock instance Generic Task
makeLabels ''Task
deriving stock instance Generic Status
makeLabels ''Status
makePrismLabels ''Status
deriving stock instance Generic (Aeson.Result a)
makePrismLabels ''Aeson.Result

instance LabelOptic "partof" A_Lens Task Task (Maybe UUID) (Maybe UUID) where
  labelOptic =
    lens
      ((^. #uda % at "partof") >=> (^? #_Success) . fromJSON)
      (\task uuid -> #uda % at "partof" .~ (toJSON <$> uuid) $ task)
instance LabelOptic "description" A_Lens TaskInfos TaskInfos Text Text where
  labelOptic = #task % #description
instance LabelOptic "uuid" A_Lens TaskInfos TaskInfos UUID UUID where
  labelOptic = #task % #uuid
instance LabelOptic "status" A_Lens TaskInfos TaskInfos Status Status where
  labelOptic = #task % #status
instance LabelOptic "tags" A_Lens TaskInfos TaskInfos (Set Text) (Set Text) where
  labelOptic = #task % #tags
instance LabelOptic "partof" A_Lens TaskInfos TaskInfos (Maybe UUID) (Maybe UUID) where
  labelOptic = #task % #partof
instance LabelOptic "modified" A_Lens TaskInfos TaskInfos (Maybe UTCTime) (Maybe UTCTime) where
  labelOptic = #task % #modified
instance LabelOptic "depends" A_Lens TaskInfos TaskInfos (Set UUID) (Set UUID) where
  labelOptic = #task % #depends
instance (R.Reflex t, c ~ R.Behavior t a) => LabelOptic "current" A_Getter (R.Dynamic t a) (R.Dynamic t a) c c where
  labelOptic = to R.current
