{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances, OverloadedLabels, TypeApplications #-}
module Frontend.Types
  ( Widget
  , ViewWidget
  , WidgetIO
  , TaskInfos(..)
  , TaskState
  , StateChange
  , DragState(..)
  , AppChange(..)
  , DataChange(..)
  , FilterState(..)
  , StandardWidget
  , AppState(AppState)
  , getTime
  , getTasks
  , getAppState
  , getFilterState
  , getDragState
  , HasAppState(appState)
  , fl
  )
where
import qualified Reflex.Dom                    as D
import qualified Reflex                        as R
import           Data.List.NonEmpty             ( NonEmpty )
import           Control.Monad.Fix              ( MonadFix )
import           Data.HashMap.Strict            ( HashMap )
import           Data.UUID                      ( UUID )
import           Taskwarrior.Task               ( Task )
import           Taskwarrior.Status             ( Status )
import           Control.Monad.Reader           ( MonadReader )
import           Data.Time.LocalTime            ( ZonedTime )
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Aeson                     ( toJSON
                                                , fromJSON
                                                )
type Widget t m
  = ( D.DomBuilder t m
    , D.DomBuilderSpace m ~ D.GhcjsDomSpace
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
    )
type WidgetIO t m
  = ( MonadIO m
    , Widget t m
    , R.TriggerEvent t m
    , R.PerformEvent t m
    , MonadIO (R.Performable m)
    )
declareFieldLabels [d|data TaskInfos = TaskInfos { task :: Task, showChildren :: Bool, children :: [UUID], parents :: [UUID]} deriving (Eq, Show)|]

type TaskState = (HashMap UUID TaskInfos)

newtype AppChange = DragChange DragState
data DataChange = ToggleEvent UUID Bool | ChangeTask Task | CreateTask Text (Task -> Task)

type StateChange = Either AppChange DataChange

data DragState = DraggedTask UUID | DraggedTag Text | NoDrag

data FilterState = FilterState { deletedFade :: NominalDiffTime,  completedFade :: NominalDiffTime}

data AppState t = AppState { _taskState :: R.Dynamic t TaskState, _currentTime :: R.Dynamic t ZonedTime , _dragState :: R.Dynamic t DragState, _filterState :: R.Dynamic t FilterState}

makeClassy ''AppState

fl :: (Functor f, Is k A_Getter) => (Optic' k is s a -> Getter (f s) (f a))
fl a = to . fmap $ view a

instance LabelOptic "partof" A_Lens Task Task (Maybe UUID) (Maybe UUID) where
  labelOptic = lens
    ((^. #uda % at "partof") >=> (^? #_Success) . fromJSON)
    (\task uuid -> #uda % at "partof" .~ (toJSON <$> uuid) $ task)
instance LabelOptic "description" A_Lens TaskInfos TaskInfos Text Text where
  labelOptic = #task % #description
instance LabelOptic "uuid" A_Lens TaskInfos TaskInfos UUID UUID where
  labelOptic = #task % #uuid
instance LabelOptic "tags" A_Lens TaskInfos TaskInfos [Text] [Text] where
  labelOptic = #task % #tags
instance LabelOptic "status" A_Lens TaskInfos TaskInfos Status Status where
  labelOptic = #task % #status
instance LabelOptic "partof" A_Lens TaskInfos TaskInfos (Maybe UUID) (Maybe UUID) where
  labelOptic = #task % #partof
instance LabelOptic "modified" A_Lens TaskInfos TaskInfos (Maybe UTCTime) (Maybe UTCTime) where
  labelOptic = #task % #modified
instance LabelOptic "depends" A_Lens TaskInfos TaskInfos [UUID] [UUID] where
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
instance (Functor f, Is k A_Getter,LabelOptic "children" k s s a a, c ~ f a, d ~ f a) => LabelOptic "children" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"children"
instance (Functor f, Is k A_Getter,LabelOptic "task" k s s a a, c ~ f a, d ~ f a) => LabelOptic "task" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"task"
instance (Functor f, Is k A_Getter,LabelOptic "parents" k s s a a, c ~ f a, d ~ f a) => LabelOptic "parents" A_Getter (f s) (f s) c d where
  labelOptic = fl $ fromLabel @"parents"
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

getFilterState
  :: (MonadReader r m, HasAppState r t) => m (R.Dynamic t FilterState)
getFilterState = asks (^. filterState)

getDragState :: (MonadReader r m, HasAppState r t) => m (R.Dynamic t DragState)
getDragState = asks (^. dragState)
