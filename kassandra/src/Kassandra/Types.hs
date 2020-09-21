module Kassandra.Types
  ( Widget
  , WidgetIO
  , TaskInfos(TaskInfos)
  , TaskState
  , AppStateChange
  , DragState(DraggedTask, NoDrag)
  , TaskTreeState
  , TaskTreeStateChange
  , DataChange(ChangeTask, CreateTask)
  , FilterState(FilterState)
  , StandardWidget
  , TaskTreeWidget
  , ExpandedTasks
  , ToggleEvent(ToggleEvent)
  , WriteApp
  , Write
  , Have
  , HaveApp
  , AppState(AppState)
  , getAppState
  , getTasks
  , getDragState
  , getFilterState
  , getTime
  , getIsExpanded
  , getExpandedTasks
  , fl
  , al
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import qualified Taskwarrior.Task
import qualified Taskwarrior.Status
import qualified Data.Aeson                    as Aeson
import           Data.HashSet                   ( member )

type Widget t m
  = ( D.DomBuilder t m
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
type WidgetIO t m = Widget t m
data TaskInfos = TaskInfos { task :: Task, children :: [UUID], parents :: [UUID], revDepends :: [UUID], blocked :: Bool} deriving stock (Eq, Show, Generic)
makeLabels ''TaskInfos

type TaskState = (HashMap UUID TaskInfos)

data DragState = DraggedTask UUID | DraggedTag Text | NoDrag deriving stock (Eq, Show, Generic)
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

data FilterState = FilterState { deletedFade :: NominalDiffTime,  completedFade :: NominalDiffTime} deriving stock (Eq, Show, Generic)
makeLabels ''FilterState

type ExpandedTasks = HashSet UUID
type TaskTreeState t = R.Dynamic t ExpandedTasks

data AppState t = AppState { taskState :: R.Dynamic t TaskState, currentTime :: R.Dynamic t ZonedTime , dragState :: R.Dynamic t DragState, filterState :: R.Dynamic t FilterState} deriving stock (Generic)
makeLabels ''AppState

type Have m r s = (MonadReader r m, HasType s r)
type HaveApp t m r = (R.Reflex t, Have m r (AppState t))
type HaveTaskTree t m r = (Have m r (TaskTreeState t))
type Write t m e s = (R.Reflex t, R.EventWriter t (NonEmpty e) m, AsType s e)
type WriteApp t m e = (Write t m e AppStateChange)
type WriteTaskTree t m e = (Write t m e TaskTreeStateChange)
type StandardWidget t m r e = (Widget t m, HaveApp t m r, WriteApp t m e)
type TaskTreeWidget t m r e
  = (StandardWidget t m r e, HaveTaskTree t m r, WriteTaskTree t m e)


getIsExpanded
  :: (Widget t m, HaveTaskTree t m r) => UUID -> m (R.Dynamic t Bool)
getIsExpanded uuid = R.holdUniqDyn . fmap (member uuid) =<< getExpandedTasks

getExpandedTasks :: HaveTaskTree t m r => m (TaskTreeState t)
getExpandedTasks = asks (^. typed)

getAppState :: (MonadReader r m, HasType (AppState t) r) => m (AppState t)
getAppState = asks (^. typed)
getTasks
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t TaskState)
getTasks = getAppState ^. al #taskState
getDragState
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t DragState)
getDragState = getAppState ^. al #dragState
getFilterState
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t FilterState)
getFilterState = getAppState ^. al #filterState
getTime
  :: (MonadReader r m, HasType (AppState t) r) => m (R.Dynamic t ZonedTime)
getTime = getAppState ^. al #currentTime

fl :: (Functor f, Is k A_Getter) => Optic' k is s a -> Getter (f s) (f a)
fl a = to (view a <$>)
al
  :: (Applicative a, Is k A_Setter, Is k A_Getter)
  => Optic' k is s b
  -> Lens' (a s) (a b)
al a = lens (view a <$>) (flip (liftA2 (set a)))

deriving stock instance Generic Task
makeLabels ''Task
deriving stock instance Generic Status
makeLabels ''Status
makePrismLabels ''Status
deriving stock instance Generic (Aeson.Result a)
makePrismLabels ''Aeson.Result


instance LabelOptic "partof" A_Lens Task Task (Maybe UUID) (Maybe UUID) where
  labelOptic = lens
    ((^. #uda % at "partof") >=> (^? #_Success) . fromJSON)
    (\task uuid -> #uda % at "partof" .~ (toJSON <$> uuid) $ task)
instance LabelOptic "description" A_Lens TaskInfos TaskInfos Text Text where
  labelOptic = (#task :: Lens' TaskInfos Task) % #description
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
instance Field1 s t a b =>LabelOptic "_1" A_Lens s t a b where
  labelOptic = _1
instance Field2 s t a b =>LabelOptic "_2" A_Lens s t a b where
  labelOptic = _2
instance (Functor f, Is k A_Getter,LabelOptic "status" k s s a a, c ~ f a, d ~ f a) => LabelOptic "status" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"status"
instance (Functor f, Is k A_Getter,LabelOptic "showChildren" k s s a a, c ~ f a, d ~ f a) => LabelOptic "showChildren" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"showChildren"
instance (Functor f, Is k A_Getter,LabelOptic "uuid" k s s a a, c ~ f a, d ~ f a) => LabelOptic "uuid" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"uuid"
instance (Functor f, Is k A_Getter,LabelOptic "description" k s s a a, c ~ f a, d ~ f a) => LabelOptic "description" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"description"
instance (Functor f, Is k A_Getter,LabelOptic "partof" k s s a a, c ~ f a, d ~ f a) => LabelOptic "partof" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"partof"
instance (Functor f, Is k A_Getter,LabelOptic "tags" k s s a a, c ~ f a, d ~ f a) => LabelOptic "tags" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"tags"
instance (Functor f, Is k A_Getter,LabelOptic "depends" k s s a a, c ~ f a, d ~ f a) => LabelOptic "depends" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"depends"
instance (R.Reflex t, c ~ R.Behavior t a) => LabelOptic "current" A_Getter (R.Dynamic t a) (R.Dynamic t a) c c where
  labelOptic = to R.current
