{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances, OverloadedLabels, TypeApplications, RankNTypes #-}
module Types
  ( Widget
  , ViewWidget
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
  , fg
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
type Widget t m
  = ( D.DomBuilder t m
    , D.DomBuilderSpace m ~ D.GhcjsDomSpace
    , MonadFix m
    , R.MonadHold t m
    , R.PostBuild t m
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

fg :: Functor f => (Optic' A_Getter is s a -> Getter (f s) (f a))
fg a = to (fmap (^. a))

fl :: Functor f => (Optic' A_Lens is s a -> Getter (f s) (f a))
fl a = to (fmap (^. a))

instance LabelOptic "uuid" A_Lens TaskInfos TaskInfos UUID UUID where
  labelOptic = #task % #uuid
instance LabelOptic "tags" A_Lens TaskInfos TaskInfos [Text] [Text] where
  labelOptic = #task % #tags
instance LabelOptic "status" A_Lens TaskInfos TaskInfos Status Status where
  labelOptic = #task % #status
instance Field1 s t a b =>LabelOptic "_1" A_Lens s t a b where
  labelOptic = _1
instance Field2 s t a b =>LabelOptic "_2" A_Lens s t a b where
  labelOptic = _2
instance (Functor f, LabelOptic "status" g s s a a, c ~ f a, d ~ f a) => LabelOptic "status" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"status")
instance (Functor f, LabelOptic "showChildren" g s s a a, c ~ f a, d ~ f a) => LabelOptic "showChildren" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"showChildren")
instance (Functor f, LabelOptic "uuid" g s s a a, c ~ f a, d ~ f a) => LabelOptic "uuid" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"uuid")
instance (Functor f, LabelOptic "children" g s s a a, c ~ f a, d ~ f a) => LabelOptic "children" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"children")
instance (Functor f, LabelOptic "task" g s s a a, c ~ f a, d ~ f a) => LabelOptic "task" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"task")
instance (Functor f, LabelOptic "parents" g s s a a, c ~ f a, d ~ f a) => LabelOptic "parents" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"parents")
instance (Functor f, LabelOptic "description" g s s a a, c ~ f a, d ~ f a) => LabelOptic "description" A_Getter (f s) (f s) c d where
  labelOptic = to (^. fromLabel @"description")
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
